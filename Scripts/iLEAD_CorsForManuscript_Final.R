# SCRIPT TO FORMAT TASK CORRELATIONS FOR MANUSCRIPT
###################################################

# Script edited by K. O'Laughlin 2021-11-22 to add annotations and edit user-specific file paths
# Script edited by K. O'Laughlin 2022-12-06 to add edit file paths for consistency


# Load libraries
library(Hmisc)
library(psycho)


# Read in data
dat = read.csv('./Scripts/Mplus CFA Inputs/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-wide_clean_trimRTAccExOutlierRMBRTRegress_prog-Mplus_date-2023-03-07.csv', #sep = ' ', 
               header = F, na.strings = 9999)

# Name columns in dat
colnames(dat) = c("pid",
                       "Cohort", "gender", "grade.1",
                       "BACKWARDSSPATIALSPAN.object_count_span.residuals.1",
                       "SPATIALSPAN.object_count_span.residuals.1",
                       "SAAT.rt_sd.correct.sustained.residuals.1",
                       "SAAT.dprime.impulsive.residuals.1",
                       "TNT.dprime.tap_trace.residuals.1",
                       "FLANKER.rcs.overall.residuals.1",
                       "STROOP.rcs.overall.residuals.1",
                       "TASKSWITCH.rcs.overall.residuals.1",
                       "BOXED.rt_mean.correct.residuals.1",
                       "grade.2",
                       "BACKWARDSSPATIALSPAN.object_count_span.residuals.2",
                       "SPATIALSPAN.object_count_span.residuals.2",
                       "SAAT.rt_sd.correct.sustained.residuals.2",
                       "SAAT.dprime.impulsive.residuals.2",
                       "TNT.dprime.tap_trace.residuals.2",
                       "FLANKER.rcs.overall.residuals.2",
                       "STROOP.rcs.overall.residuals.2",
                       "TASKSWITCH.rcs.overall.residuals.2",
                       "BOXED.rt_mean.correct.residuals.2",
                       "grade.3",
                       "BACKWARDSSPATIALSPAN.object_count_span.residuals.3",
                       "SPATIALSPAN.object_count_span.residuals.3",
                       "SAAT.rt_sd.correct.sustained.residuals.3",
                       "SAAT.dprime.impulsive.residuals.3",
                       "TNT.dprime.tap_trace.residuals.3",
                       "FLANKER.rcs.overall.residuals.3",
                       "STROOP.rcs.overall.residuals.3",
                       "TASKSWITCH.rcs.overall.residuals.3",
                       "BOXED.rt_mean.correct.residuals.3",
                       "grade.4",
                       "BACKWARDSSPATIALSPAN.object_count_span.residuals.4",
                       "SPATIALSPAN.object_count_span.residuals.4",
                       "SAAT.rt_sd.correct.sustained.residuals.4",
                       "SAAT.dprime.impulsive.residuals.4",
                       "TNT.dprime.tap_trace.residuals.4",
                       "FLANKER.rcs.overall.residuals.4",
                       "STROOP.rcs.overall.residuals.4",
                       "TASKSWITCH.rcs.overall.residuals.4",
                       "BOXED.rt_mean.correct.residuals.4"
)


# Remove unneeded columns from dat
dat = dat[, -c(seq(12, ncol(dat), 10))]
dat = dat[, -c(seq(4, ncol(dat), 9))]
dat = dat[, -3]

# Rename columns of dat
colnames(dat) = c(colnames(dat)[1:2], 'BSS1', 'FSS1', 'SSUS1', 'SIMP1', 'TNT1', 'FLNK1', 'STRP1', 'BOX1',
                  'BSS2', 'FSS2', 'SSUS2', 'SIMP2', 'TNT2', 'FLNK2', 'STRP2', 'BOX2',
                  'BSS3', 'FSS3', 'SSUS3', 'SIMP3', 'TNT3', 'FLNK3', 'STRP3', 'BOX3',
                  'BSS4', 'FSS4', 'SSUS4', 'SIMP4', 'TNT4', 'FLNK4', 'STRP4', 'BOX4')


# Transform Scales of Boxed and Sustained Attention at each time
dat$BOX1 = -1*dat$BOX1/100
dat$SSUS1 = -1*dat$SSUS1/100

dat$BOX2 = -1*dat$BOX2/100
dat$SSUS2 = -1*dat$SSUS2/100

dat$BOX3 = -1*dat$BOX3/100
dat$SSUS3 = -1*dat$SSUS3/100

dat$BOX4 = -1*dat$BOX4/100
dat$SSUS4 = -1*dat$SSUS4/100


# Subset relevant columns for correlations for each cohort
cor_dat3 = dat[dat$Cohort == 3, -c(1:2)]
cor_dat5 = dat[dat$Cohort == 5, -c(1:2)]
cor_dat7 = dat[dat$Cohort == 7, -c(1:2)]


# correlation_matrix function found at https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/
source('./Functions/FormatCorrelationMatrix_function.R')

# Use correlation_matrix()
cor_g3 = correlation_matrix(cor_dat3, use = 'lower', replace_diagonal = T)
cor_g5 = correlation_matrix(cor_dat5, use = 'lower', replace_diagonal = T)
cor_g7 = correlation_matrix(cor_dat7, use = 'lower', replace_diagonal = T)


# Write results to .csv files
write.csv(cor_g3, file = paste0('./Results/iLEAD_Corrs_AllTimes_3rdGrade_', Sys.Date(), '.csv'))
write.csv(cor_g5, file = paste0('./Results/iLEAD_Corrs_AllTimes_5thGrade_', Sys.Date(), '.csv'))
write.csv(cor_g7, file = paste0('./Results/iLEAD_Corrs_AllTimes_7thGrade_', Sys.Date(), '.csv'))

# Get descriptive statistics of task performance
M3 = round(sapply(cor_dat3, mean, na.rm = T), 3)
SD3 = round(sapply(cor_dat3, sd, na.rm = T), 3)
des_g3 = t(data.frame(Mean = M3, SD = SD3))

M5 = round(sapply(cor_dat5, mean, na.rm = T), 3)
SD5 = round(sapply(cor_dat5, sd, na.rm = T), 3)
des_g5 = t(data.frame(Mean = M5, SD = SD3))

M7 = round(sapply(cor_dat7, mean, na.rm = T), 3)
SD7 = round(sapply(cor_dat7, sd, na.rm = T), 3)
des_g7 = t(data.frame(Mean = M7, SD = SD7))


# Write descriptive statistics to .csv files 
write.csv(des_g3, file = paste0('./Results/iLEAD_MeansSDs_3rdGrade_', Sys.Date(), '.csv'))
write.csv(des_g5, file = paste0('./Results/iLEAD_MeansSDs_5thGrade_', Sys.Date(), '.csv'))
write.csv(des_g7, file = paste0('./Results/iLEAD_MeansSDs_7thGrade_', Sys.Date(), '.csv'))

