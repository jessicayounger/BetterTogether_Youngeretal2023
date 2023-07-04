#Function to replace data with 'NA' for subjects with performance outside of a specified number of mads away from median or sd away from mean
#Data from each subject is individually evaluated for performance for each module (i.e. only the below chance module data will be replaced for a given subject)
#User can specify the cut_off values for each type of task 
#This is the not as well written version that was used in the initial analyses. Including this version for exact replication purposes
#All modules are evaluated
#Jessica Younger 02/17/2021

ACE_rm_outliers = function(df,mad_cutoff) {
  
  df$TNT.outlier=ifelse(df$TNT.sdt_dprime.tap_trace > ((mad_cutoff*df$TNT.sdt_dprime.tap_trace_mad) + df$TNT.sdt_dprime.tap_trace_median) | (df$TNT.sdt_dprime.tap_trace< (df$TNT.sdt_dprime.tap_trace_median- (mad_cutoff*df$TNT.sdt_dprime.tap_trace_mad))), 1, 0)
  df$FLANKER.outlier=ifelse(df$FLANKER.rcs.overall > ((mad_cutoff*df$FLANKER.rcs.overall_mad) + df$FLANKER.rcs.overall_median) | df$FLANKER.rcs.overall < (df$FLANKER.rcs.overall_median- (mad_cutoff*df$FLANKER.rcs.overall_mad)), 1, 0)
  df$STROOP.outlier=ifelse(df$STROOP.rcs.overall > ((mad_cutoff*df$STROOP.rcs.overall_mad) + df$STROOP.rcs.overall_median) | df$STROOP.rcs.overall < (df$STROOP.rcs.overall_median-(mad_cutoff*df$STROOP.rcs.overall_mad)), 1, 0)
  #df$TASKSWITCH.outlier=ifelse(df$TASKSWITCH.rcs.overall > ((mad_cutoff*df$TASKSWITCH.rcs.overall_mad) + df$TASKSWITCH.rcs.overall_median) | df$TASKSWITCH.rcs.overall < ( df$TASKSWITCH.rcs.overall_median-(mad_cutoff*df$TASKSWITCH.rcs.overall_mad)), 1, 0)
  df$BOXED.outlier=ifelse(df$BOXED.rt_mean.correct > ((mad_cutoff*df$BOXED.rt_mean.correct_mad) + df$BOXED.rt_mean.correct_median) | df$BOXED.rt_mean.correct < (df$BOXED.rt_mean.correct_median-(mad_cutoff*df$BOXED.rt_mean.correct_mad)), 1, 0)
  df$BRT.outlier=ifelse(df$BRT.rt_mean.correct > ((mad_cutoff*df$BRT.rt_mean.correct_mad) + df$BRT.rt_mean.correct_median) | df$BRT.rt_mean.correct < (df$BRT.rt_mean.correct_median-(mad_cutoff*df$BRT.rt_mean.correct_mad)), 1, 0)
  df$BACKWARDSSPATIALSPAN.outlier=ifelse(df$BACKWARDSSPATIALSPAN.object_count_span.overall > ((mad_cutoff*df$BACKWARDSSPATIALSPAN.object_count_span.overall_mad) + df$BACKWARDSSPATIALSPAN.object_count_span.overall_median) | df$BACKWARDSSPATIALSPAN.object_count_span.overall < (df$BACKWARDSSPATIALSPAN.object_count_span.overall_median-(mad_cutoff*df$BACKWARDSSPATIALSPAN.object_count_span.overall_mad)), 1, 0)
  df$SPATIALSPAN.outlier=ifelse(df$SPATIALSPAN.object_count_span.overall > ((mad_cutoff*df$SPATIALSPAN.object_count_span.overall_mad) + df$SPATIALSPAN.object_count_span.overall_median) | df$SPATIALSPAN.object_count_span.overall < (df$SPATIALSPAN.object_count_span.overall_median-(mad_cutoff*df$SPATIALSPAN.object_count_span.overall_mad)), 1, 0)
  df$SAAT.imp.outlier=ifelse(df$SAATIMPULSIVE.sdt_dprime.overall > ((mad_cutoff*df$SAATIMPULSIVE.sdt_dprime.overall_mad) + df$SAATIMPULSIVE.sdt_dprime.overall_median) | df$SAATIMPULSIVE.sdt_dprime.overall < (df$SAATIMPULSIVE.sdt_dprime.overall_median-(mad_cutoff*df$SAATIMPULSIVE.sdt_dprime.overall_mad)), 1, 0)
  df$SAAT.sus.outlier=ifelse(df$SAATSUSTAINED.rt_sd.correct > ((mad_cutoff*df$SAATSUSTAINED.rt_sd.correct_mad) + df$SAATSUSTAINED.rt_sd.correct_median) | df$SAATSUSTAINED.rt_sd.correct < (df$SAATSUSTAINED.rt_sd.correct_median-(mad_cutoff*df$SAATSUSTAINED.rt_sd.correct_mad)), 1, 0)
  

if(dim(df[!is.na(df$TNT.outlier) & df$TNT.outlier==1, grep("TNT", names(df))])[1]>0){is.na(df[!is.na(df$TNT.outlier) & df$TNT.outlier==1, grep("TNT", names(df))])=TRUE}
if(dim(df[!is.na(df$FLANKER.outlier) & df$FLANKER.outlier==1, grep("FLANKER", names(df))])[1]>0){is.na(df[!is.na(df$FLANKER.outlier) & df$FLANKER.outlier==1, grep("FLANKER", names(df))])=TRUE}
if(dim(df[!is.na(df$STROOP.outlier) & df$STROOP.outlier==1, grep("STROOP", names(df))])[1]>0){is.na(df[!is.na(df$STROOP.outlier) & df$STROOP.outlier==1, grep("STROOP", names(df))])=TRUE}
#if(dim(df[!is.na(df$TASKSWITCH.outlier) & df$TASKSWITCH.outlier==1, grep("TASKSWITCH", names(df))])[1]>0){is.na(df[!is.na(df$TASKSWITCH.outlier) & df$TASKSWITCH.outlier==1, grep("TASKSWITCH", names(df))])=TRUE}
if(dim(df[!is.na(df$BOXED.outlier) & df$BOXED.outlier==1, grep("BOXED", names(df))])[1]>0){is.na(df[!is.na(df$BOXED.outlier) & df$BOXED.outlier==1, grep("BOXED", names(df))])=TRUE}
if(dim(df[!is.na(df$BRT.outlier) & df$BRT.outlier==1, grep("BRT", names(df))])[1]>0){is.na(df[!is.na(df$BRT.outlier) & df$BRT.outlier==1, grep("BRT", names(df))])=TRUE}
if(dim(df[!is.na(df$BACKWARDSSPATIALSPAN.outlier) & df$BACKWARDSSPATIALSPAN.outlier==1, grep("BACKWARDSSPATIALSPAN", names(df))])[1]>0){is.na(df[!is.na(df$BACKWARDSSPATIALSPAN.outlier) & df$BACKWARDSSPATIALSPAN.outlier==1, grep("BACKWARDSSPATIALSPAN", names(df))])=TRUE}
if(dim(df[!is.na(df$SPATIALSPAN.outlier) & df$SPATIALSPAN.outlier==1, grep("SPATIALSPAN", names(df))])[1]>0){is.na(df[!is.na(df$SPATIALSPAN.outlier) & df$SPATIALSPAN.outlier==1, grep("SPATIALSPAN", names(df))])=TRUE}
if(dim(df[!is.na(df$SAAT.imp.outlier) & df$SAAT.imp.outlier==1, grep("IMPULSIVE", names(df))])[1]>0){is.na(df[!is.na(df$SAAT.imp.outlier) & df$SAAT.imp.outlier==1, grep("IMPULSIVE", names(df))])=TRUE}
if(dim(df[!is.na(df$SAAT.sus.outlier) & df$SAAT.sus.outlier==1, grep("SUSTAINED", names(df))])[1]>0){is.na(df[!is.na(df$SAAT.sus.outlier) & df$SAAT.sus.outlier==1, grep("SUSTAINED", names(df))])=TRUE}
  
   return(df)}
