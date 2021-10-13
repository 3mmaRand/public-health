library(NHANES)
library(RNHANES)
library(tidyverse)


# proportions representing a simple random sample
prop <- as.numeric(table(NHANES$Race1)/nrow(NHANES))

set.seed(1000) # reproducible

# take sample from NHANESraw that represents a simple random sample
dat <- NHANESraw %>%
  
  # add sample weights
  mutate(weight = case_when(Race1 == "Black" ~ prop[1],
                            Race1 == "Hispanic" ~ prop[2],
                            Race1 == "Mexican" ~ prop[3],
                            Race1 == "White" ~ prop[4],
                            Race1 == "Other" ~ prop[5])) %>%
  group_by(Race1) %>%
  sample_n(3000 * weight) %>% # sample from each according to prop to obtain 10000 obvs in total
  rename(Sex = Gender) %>%
  ungroup(Race1) %>% 
  select(-c(weight, 
            WTINT2YR, WTMEC2YR, 
            SDMVPSU, SDMVSTRA)) %>% # remove weighting columns
  select(-c(SurveyYr,
            BMI_WHO,
            HHIncomeMid,
            Length,
            HeadCirc,
            DiabetesAge,
            BMICatUnder20yrs,
            BPSys1, BPSys2,
            BPDia1, BPDia2,
            BPSys3, BPDia3,
            UrineVol2,
            UrineFlow2,
            PregnantNow,
            Marijuana,
            RegularMarij,
            AgeFirstMarij,
            AgeRegMarij,
            HardDrugs,
            SexEver,
            SexAge,
            SexNumPartnLife,
            SexNumPartYear,
            SameSex,
            SexOrientation,
            nPregnancies,
            nBabies,
            Age1stBaby,
            AgeMonths,
            Race1,
            HHIncome,
            Poverty,
            HomeRooms,
            HomeOwn,
            Education,
            MaritalStatus,
            Work)) %>% # remove variables which will not be used
  select(-c(Race3, 
            Testosterone,
            TVHrsDay, 
            CompHrsDay,
            TVHrsDayChild,
            CompHrsDayChild)) # remove data which was only recorded for 
  # one out of two survey rounds


# Add FEV1 variable
dat <- nhanes_load_data(c("SPX_F"), "2009-2010") %>%
  select(SEQN, SPXNFEV1) %>%
  bind_rows(nhanes_load_data(c("SPX_G"), "2011-2012") %>% 
              select(SEQN, SPXNFEV1)) %>%
  filter(SEQN %in% dat$ID) %>%
  rename(FEV1 = SPXNFEV1) %>%
  right_join(dat, by = c("SEQN" = "ID")) %>%
  rename(ID = SEQN)

dat <- dat %>% 
  filter(Age > 19) %>%
  filter(Age < 75) %>%
  filter(!is.na(FEV1)) %>%
  filter(!is.na(Weight)) %>%
  filter(!is.na(Height)) %>%
  filter(!is.na(Pulse))  %>%
  filter(!is.na(BPSysAve))  %>%
  filter(!is.na(BPDiaAve)) %>%
  filter(!is.na(DirectChol)) %>%
  filter(!is.na(TotChol))  %>%
  filter(!is.na(UrineVol1))  %>%
  filter(!is.na(UrineFlow1))  %>%
  filter(!is.na(Diabetes)) %>%
  filter(!is.na(HealthGen)) %>%
  filter(!is.na(DaysPhysHlthBad)) %>%
  filter(!is.na(DaysMentHlthBad)) %>%
  filter(!is.na(LittleInterest   ))  %>%
  filter(!is.na(Depressed))  %>%
  filter(!is.na(SleepHrsNight))  %>%
  filter(!is.na(SleepTrouble))  %>%
  filter(!is.na(Alcohol12PlusYr)) %>%
  filter(!is.na(Smoke100))

summary(dat)

write_csv(dat, file = "NHANES.csv")
