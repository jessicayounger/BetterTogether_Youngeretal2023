#Function to replace data with 'NA' for subjects with trials less than a specified number for each condition
#Data from each subject is individually evaluated for performance for each module (i.e. only the below chance module data will be replaced for a given subject)
#User can specify the cut_off number of trials and it will apply to all modules
#Jessica Younger 11/02/2018
#updated 11/7/2018 to include the 'if exists' to make function robust to data sets without all modules
ACE_remove_lowcount = function(df, minimum_ntrials) {
  #Convert relevant data to numeric
  if("STROOP.rt_count.congruent" %in% colnames(df)){df$STROOP.rt_count.congruent=as.numeric(df$STROOP.rt_count.congruent)}
  if("STROOP.rt_count.incongruent" %in% colnames(df)){df$STROOP.rt_count.incongruent=as.numeric(df$STROOP.rt_count.incongruent)}
  if("FLANKER.rt_count.congruent" %in% colnames(df)){df$FLANKER.rt_count.congruent=as.numeric(df$FLANKER.rt_count.congruent)}
  if("FLANKER.rt_count.incongruent" %in% colnames(df)){df$FLANKER.rt_count.incongruent=as.numeric(df$FLANKER.rt_count.incongruent)}
  if("TASKSWITCH.rt_count.stay" %in% colnames(df)){ df$TASKSWITCH.rt_count.stay=as.numeric(df$TASKSWITCH.rt_count.stay)}
  if("TASKSWITCH.rt_count.switch" %in% colnames(df)){df$TASKSWITCH.rt_count.switch=as.numeric(df$TASKSWITCH.rt_count.switch)}
  if("SAATSUSTAINED.rt_count" %in% colnames(df)){df$SAATSUSTAINED.rt_count.overall=as.numeric(df$SAATSUSTAINED.rt_count.overall)}
  if("SAATIMPULSIVE.rt_count" %in% colnames(df)){df$SAATIMPULSIVE.rt_count.overall=as.numeric(df$SAATIMPULSIVE.rt_count.overall)}
  if("TNT.rt_count.tap_only" %in% colnames(df)){ df$TNT.rt_count.tap_only=as.numeric(df$TNT.rt_count.tap_only)}
  if("TNT.rt_count.tap_trace" %in% colnames(df)){ df$TNT.rt_count.tap_trace=as.numeric(df$TNT.rt_count.tap_trace)}
  if("BOXED.rt_count.conjunction_12" %in% colnames(df)){ df$BOXED.rt_count.conjunction_12=as.numeric(df$BOXED.rt_count.conjunction_12)}
  if("BOXED.rt_count.feature_4" %in% colnames(df)){df$BOXED.rt_count.feature_4=as.numeric(df$BOXED.rt_count.feature_4)}
  if("BOXED.rt_count.feature_12" %in% colnames(df)){ df$BOXED.rt_count.feature_12=as.numeric(df$BOXED.rt_count.feature_12)}
  if("BOXED.rt_count.conjunction_4" %in% colnames(df)){ df$BOXED.rt_count.conjunction_4=as.numeric(df$BOXED.rt_count.conjunction_4)}
  if("FILTER.rt_count.R2B0" %in% colnames(df)){ df$FILTER.rt_count.R2B0=as.numeric(df$FILTER.rt_count.R2B0)}
  if("FILTER.rt_count.R2B2" %in% colnames(df)){ df$FILTER.rt_count.R2B2=as.numeric(df$FILTER.rt_count.R2B2)}
  if("FILTER.rt_count.R2B4" %in% colnames(df)){ df$FILTER.rt_count.R2B4=as.numeric(df$FILTER.rt_count.R2B4)}
  if("FILTER.rt_count.R4B2" %in% colnames(df)){ df$FILTER.rt_count.R4B2=as.numeric(df$FILTER.rt_count.R4B2)}
  if("FILTER.rt_count.R4B0" %in% colnames(df)){ df$FILTER.rt_count.R4B0=as.numeric(df$FILTER.rt_count.R4B0)}
  
  
  if(dim(is.na(df[!is.na(df$TNT.rt_count.tap_only) & df$TNT.rt_count.tap_only<minimum_ntrials, grep("TNT", names(df))]))[1]>0){is.na(df[!is.na(df$TNT.rt_count.tap_only) & df$TNT.rt_count.tap_only<minimum_ntrials, grep("TNT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATSUSTAINED.rt_count.overall) & df$SAATSUSTAINED.rt_count.overall<minimum_ntrials, grep("SAATSUSTAINED", names(df))]))[1]>0){is.na(df[!is.na(df$SAATSUSTAINED.rt_count.overall) & df$SAATSUSTAINED.rt_count.overall<minimum_ntrials, grep("SAAT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATIMPULSIVE.rt_count.overall) & df$SAATIMPULSIVE.rt_count.overall<minimum_ntrials, grep("SAATIMPULSIVE", names(df))]))[1]>0){is.na(df[!is.na(df$SAATIMPULSIVE.rt_count.overall) & df$SAATIMPULSIVE.rt_count.overall<minimum_ntrials, grep("SAAT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$STROOP.rt_count.congruent) & df$STROOP.rt_count.congruent<minimum_ntrials, grep("STROOP", names(df))]))[1]>0){is.na(df[!is.na(df$STROOP.rt_count.congruent) & df$STROOP.rt_count.congruent<=minimum_ntrials, grep("STROOP", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FLANKER.rt_count.congruent) & df$FLANKER.rt_count.congruent<minimum_ntrials, grep("FLANKER", names(df))]))[1]>0){is.na(df[!is.na(df$FLANKER.rt_count.congruent) & df$FLANKER.rt_count.congruent<minimum_ntrials, grep("FLANKER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$TASKSWITCH.rt_count.stay) & df$TASKSWITCH.rt_count.stay<minimum_ntrials, grep("TASKSWITCH", names(df))]))[1]>0){is.na(df[!is.na(df$TASKSWITCH.rt_count.stay) & df$TASKSWITCH.rt_count.stay<minimum_ntrials, grep("TASKSWITCH", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.rt_count.R2B0) & df$FILTER.rt_count.R2B0<minimum_ntrials, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.rt_count.R2B0) & df$FILTER.rt_count.R2B0<minimum_ntrials, grep("FILTER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.rt_count.R4B0) & df$FILTER.rt_count.R4B0<minimum_ntrials, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.rt_count.R4B0) & df$FILTER.rt_count.R4B0<minimum_ntrials, grep("R4", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.rt_count.R2B2) & df$FILTER.rt_count.R2B2<minimum_ntrials, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.rt_count.R2B2) & df$FILTER.rt_count.R2B2<minimum_ntrials, grep("FILTER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.rt_count.R4B2) & df$FILTER.rt_count.R4B2<minimum_ntrials, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.rt_count.R4B2) & df$FILTER.rt_count.R4B2<minimum_ntrials, grep("R4", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.rt_count.R2B4) & df$FILTER.rt_count.R2B4<minimum_ntrials, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.rt_count.R2B4) & df$FILTER.rt_count.R2B4<minimum_ntrials, grep("FILTER", names(df))])=TRUE}
  
  if(dim(is.na(df[!is.na(df$TNT.rt_count.tap_trace) & df$TNT.rt_count.tap_trace<minimum_ntrials, grep("TNT", names(df))]))[1]>0){is.na(df[!is.na(df$TNT.rt_count.tap_trace) & df$TNT.rt_count.tap_trace<minimum_ntrials, grep("TNT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$STROOP.rt_count.incongruent) & df$STROOP.rt_count.incongruent<minimum_ntrials, grep("STROOP", names(df))]))[1]>0){is.na(df[!is.na(df$STROOP.rt_count.incongruent) & df$STROOP.rt_count.incongruent<minimum_ntrials, grep("STROOP", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FLANKER.rt_count.incongruent) & df$FLANKER.rt_count.incongruent<minimum_ntrials, grep("FLANKER", names(df))]))[1]>0){is.na(df[!is.na(df$FLANKER.rt_count.incongruent) & df$FLANKER.rt_count.incongruent<minimum_ntrials, grep("FLANKER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$TASKSWITCH.rt_count.switch) & df$TASKSWITCH.rt_count.switch<minimum_ntrials, grep("TASKSWITCH", names(df))]))[1]>0){is.na(df[!is.na(df$TASKSWITCH.rt_count.switch) & df$TASKSWITCH.rt_count.switch<minimum_ntrials, grep("TASKSWITCH", names(df))])=TRUE}
  
  if(dim(is.na(df[!is.na(df$BOXED.rt_count.feature_4) & df$BOXED.rt_count.feature_4<minimum_ntrials, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.rt_count.feature_4) & df$BOXED.rt_count.feature_4<minimum_ntrials, grep("BOXED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$BOXED.rt_count.feature_12) & df$BOXED.rt_count.feature_12<minimum_ntrials, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.rt_count.feature_12) & df$BOXED.rt_count.feature_12<minimum_ntrials, grep("BOXED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$BOXED.rt_count.conjunction_12) & df$BOXED.rt_count.conjunction_12<minimum_ntrials, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.rt_count.conjunction_12) & df$BOXED.rt_count.conjunction_12<minimum_ntrials, grep("BOXED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$BOXED.rt_count.conjunction_4) & df$BOXED.rt_count.conjunction_4<minimum_ntrials, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.rt_count.conjunction_4) & df$BOXED.rt_count.conjunction_4<minimum_ntrials, grep("BOXED", names(df))])=TRUE}
  
  
  return(df)}
