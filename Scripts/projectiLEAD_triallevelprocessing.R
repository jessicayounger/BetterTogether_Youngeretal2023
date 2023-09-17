#Script to go from trial level data to task level data for all timepoints. 
#Includes cleaning as described in the manuscript
#Includes results of N removed per cleaning step as reported in Table S1
#Last updated by Jessica Younger 2023-06-30

#install the classroom branch of aceR. If you are switching between versions, you need to restart your R session before library-ing aceR again
devtools::install_github("joaquinanguera/aceR")

#Library packages used in this script
library(tidyverse)
library(aceR)
library(lme4)

#set the working directory to where all of the files are stored
setwd("~/Desktop/BetterTogether_Scripts_Deidentify")

#Load functions used in this script
source('Functions/weighted_k_function.R')
source("Functions/Rm_Outliers_Function.R")
source("Functions/ACE_Rm_LowTrialCounts_Function.R")
source("Functions/Rm_Chance_Accuracy.R")
source("Functions/ACE_triallevelprocessing_Function.R")
source("Functions/GetCleanedDataCounts_Function.R")


#Load Data used in this script
dataT1_raw=read.csv(file="Data/t1_ACE_All_correctednamesdemo_rmdups_consent_2023-09-09.csv")
dataT2_raw=read.csv(file="Data/t2_ACE_All_correctednamesdemo_rmdups_consent_2023-09-09.csv")
dataT3_raw=read.csv(file="Data/t3_ACE_All_correctednamesdemo_rmdups_consent_2023-09-09.csv")
dataT4_raw=read.csv(file="Data/t4_ACE_All_correctednamesdemo_rmdups_consent_2023-09-09.csv")
db=read.csv(file="Data/Database_ImputedAges_RemoveNames_2023-09-09.csv")
names(dataT4_raw)
####Process the raw ACE data and provide a timepoint label
dataT1= ACE_process_trialleveldata(dataT1_raw, cutoff_min = 200, metric_names =  c("rt_count", "rt_mean.correct","rt_sd.correct", "acc_mean","object_count_span.overall", "dprime", "rcs"))
dataT1$Time.Point="T1_Fall2016"

dataT2= ACE_process_trialleveldata(dataT2_raw, cutoff_min = 200, metric_names =  c("rt_count", "rt_mean.correct","rt_sd.correct", "acc_mean","object_count_span.overall", "dprime", "rcs"))
dataT2$Time.Point="T2_Spring2017"

dataT3= ACE_process_trialleveldata(dataT3_raw, cutoff_min = 200, metric_names =  c("rt_count", "rt_mean.correct","rt_sd.correct", "acc_mean","object_count_span.overall", "dprime", "rcs"))
dataT3$Time.Point="T3_Fall2017"

dataT4= ACE_process_trialleveldata(dataT4_raw, cutoff_min = 200, metric_names =  c("rt_count", "rt_mean.correct","rt_sd.correct", "acc_mean","object_count_span.overall", "dprime", "rcs"), late_incorrect=FALSE)
dataT4$Time.Point="T4_Spring2018"

#Bind the separate timepoints together into one big data frame
Y1=bind_rows(dataT1,dataT2)
Y2=bind_rows(dataT3,dataT4)
All=bind_rows(Y1,Y2)

####Correct an issue where all students are considered to be left hand dominant in the processing output script
Righthanders=All%>%
  filter(handedness=="RIGHT")%>%
  setNames(gsub("right", "dominant", names(.)))%>%
  setNames(gsub("left", "nondominant", names(.)))

Lefthanders=All%>%
  filter(handedness=="LEFT")%>%
  setNames(gsub("left", "dominant", names(.)))%>%
  setNames(gsub("right", "nondominant", names(.)))

All_2=rbind(Righthanders, Lefthanders)

#Create Time as numeric variable for prepataion for merging with Database data
All_2$Time.Point=as.numeric(as.factor(All_2$Time.Point))

#Merge with additional demograpahic variables available in the Database
data=merge(All_2, db, by=c("pid", "Time.Point"))
#Remove instances/pids where only district data but no ACE data is available
data=data%>%
  filter(!is.na(Time.Point))

####################Begin Data Cleaning/Outlier Removal###################
#Exclude the FILTER task due to differential challenge level across Cohorts
data=data%>%
  select(-contains("FILTER")) 
#Due to technical error, TaskSwitch data from T1 is not calculated consistently, so make NA for all subjects
is.na(data[data$Time.Point==1, grep("TASKSWITCH", names(data))])=TRUE

#Removing instances where there are not at least 5 trials per condition
dataclean=ACE_remove_lowcount(data, minimum_ntrials=5)

#Removing instances where performance is at or below chance performance on the easiest condition of the module
data_chance=ACE_remove_chance(dataclean, data_type="easy", dprime_cutoff=0, twochoice_cutoff=0.5, fourchoice_cutoff=0.25)

#Removing variables with 'count' in the name as those were only needed for the lowcount function 
data_chance=data_chance%>%
  select(-contains("count."))

#Ensuring instances where mean BRT is < 200ms (indicating button mashing)
is.na(data_chance$BRT.rt_mean.correct.dominant[!is.na(data_chance$BRT.rt_mean.correct.dominant) &data_chance$BRT.rt_mean.correct.dominant <200])=TRUE

data_initialclean=data_chance

#Create data frame with median and mad for each metric of interest for each cohort and each time point
outliers=data_initialclean%>%
  group_by(Cohort, Time.Point)%>%
  select(BOXED.rt_mean.correct, SAATSUSTAINED.rt_sd.correct, SAATIMPULSIVE.sdt_dprime.overall, BRT.rt_mean.correct, STROOP.rcs.overall, TASKSWITCH.rcs.overall, FLANKER.rcs.overall, TNT.sdt_dprime.tap_trace, SPATIALSPAN.object_count_span.overall, BACKWARDSSPATIALSPAN.object_count_span.overall)%>%
  summarize_at(vars(-group_cols()), list(~median(.,na.rm=T), ~mad(., na.rm=T)))

#Merge in the median/mad metrics with the main data frame so that each line of data has the necessary reference data 
data_reduced_outliers=merge(data_initialclean, outliers, by=c("Cohort", "Time.Point"), all.x=TRUE)

#Use ACE_rm_outliers function to remove inistances where performance is 3 MADs away from Median performance 
data_clean=ACE_rm_outliers(data_reduced_outliers, mad_cutoff=3)

#Arrange the data for easier viewing
data_clean=data_clean%>%
  arrange(pid, Time.Point)
#Create variable called playcount to accurately capture times played game 
data_clean=data_clean%>%
  group_by(pid)%>%
  mutate(playcount=1:length(Time.Point))%>%
  arrange(pid)%>%
  ungroup()

#Create variable called time_since_firstgameplay to track months between game plays for each play
data_clean=data_clean%>%
  group_by(pid)%>%
  mutate(initial_age=age_months[playcount==1])%>%
  mutate(time_since_firstgameplay=age_months-initial_age)%>%
  ungroup()


#Saving all of these data to new data frame (for comparison later)
compiled=data_clean

####Ensure variables are in the correct format####
#Cohort should be factor (not numeric)
compiled$Cohort=as.factor(compiled$Cohort)


#Remove extreme outlier subject
compiled=compiled%>%
  filter(!pid=="ADMIN-UCSF-dd409")
#Remove unusued levels
compiled=droplevels(compiled)

####Begin using Cook's d to determine highly influential data points and remove####
#For each module/metric of interest, calculate lmer and determine cooksd for that model
#Determine influential observations by finding instances where cooksd > 1, and remove them 
#Stroop
mod <- lmer(STROOP.rcs.overall ~time_since_firstgameplay * age_months + gender+Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("STROOP", names(compiled))])=TRUE


#TaskSwitch
mod <- lmer(TASKSWITCH.rcs.overall ~time_since_firstgameplay * age_months + gender+Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("TASKSWITCH", names(compiled))])=TRUE

#Flanker
mod <- lmer(FLANKER.rcs.overall ~time_since_firstgameplay * age_months+ gender +Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("FLANKER", names(compiled))])=TRUE

#Backward Spatial Span
mod <- lmer(BACKWARDSSPATIALSPAN.object_count_span.overall ~time_since_firstgameplay * age_months + gender  +Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("BACKWARDSPATIALSPAN", names(compiled))])=TRUE
#Note: error occurs, as there are no influential observations

#Spatial Span
mod <- lmer(SPATIALSPAN.object_count_span.overall ~time_since_firstgameplay * age_months+ gender +Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("SPATIALSPAN", names(compiled))])=TRUE

#Boxed
mod <- lmer(BOXED.rt_mean.correct ~time_since_firstgameplay * age_months +gender +Cohort+ BRT.rt_mean.correct + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("BOXED", names(compiled))])=TRUE

#TNT
mod <- lmer(TNT.sdt_dprime.tap_trace ~time_since_firstgameplay * age_months+ gender +Cohort+ BRT.rt_mean.correct.dominant + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("TNT", names(compiled))])=TRUE

#SAAT impulsive
mod <- lmer(SAATIMPULSIVE.sdt_dprime.overall ~time_since_firstgameplay * age_months +  gender  +Cohort+ BRT.rt_mean.correct.dominant + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("IMPULSIVE", names(compiled))])=TRUE

#SAAT sustained
mod <- lmer(SAATSUSTAINED.rt_sd.correct ~time_since_firstgameplay * age_months  + gender +Cohort+ BRT.rt_mean.correct.dominant + (1 + time_since_firstgameplay|pid) + (1|School), data=compiled)
cooksd <- cooks.distance(mod)
influential_check <- as.numeric(names(cooksd)[(cooksd > 1)])  # influential row numbers
is.na(compiled[influential_check, grep("SUSTAINED", names(compiled))])=TRUE


#######Final Variable Selection########
final_data=compiled%>%
  select(pid, Cohort, Time.Point, grade, gender, contains ("span"),SAATSUSTAINED.rt_sd.correct, SAATIMPULSIVE.sdt_dprime.overall, TNT.sdt_dprime.tap_trace, contains("rcs.overall"),BOXED.rt_mean.correct, BRT.rt_mean.correct, BRT.rt_mean.correct.dominant, BRT.rt_mean.correct.nondominant,  School, contains("time"), age_months, Birthdate_provided)

#Further reduction of variables
final_data=final_data%>%
  select(-contains("outlier"), -contains("BOXED.rcs"), -contains("_mad"), -contains("_median"),  BOXED.rt_mean.correct, BRT.rt_mean.correct, BRT.rt_mean.correct.dominant, BRT.rt_mean.correct.nondominant)
#Create a dataframe with just the metrics for use in the next for loop
data_names=final_data%>%
  select(BACKWARDSSPATIALSPAN.object_count_span.overall:BOXED.rt_mean.correct)
#For loop to create a new variables that is the residual performance after BRT is regressed out
for (i in names(data_names) ){
  final_data[[paste(i, ".residuals", sep="")]]=residuals(lm(final_data[[i]] ~ final_data$BRT.rt_mean.correct, data = final_data, na.action=na.exclude))
}


###Write out the dataset for future use
write.csv(final_data, file=paste("Data/proj-InitialPapers_asmt-A_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-", Sys.Date(), ".csv", sep=""), row.names=F)
names(final_data)
####Get Counts of DataSets remaining after each cleaning step (Table S1) ####
nremoved=ACE_datasetscleaning(variables=c("STROOP.rcs.overall", "FLANKER.rcs.overall", "SPATIALSPAN.object_count_span.overall", "BACKWARDSSPATIALSPAN.object_count_span.overall",
                                          "SAATSUSTAINED.rt_sd.correct", "SAATIMPULSIVE.sdt_dprime.overall", "TNT.sdt_dprime.tap_trace", "TASKSWITCH.rcs.overall", "BOXED.rt_mean.correct"))
nremoved
