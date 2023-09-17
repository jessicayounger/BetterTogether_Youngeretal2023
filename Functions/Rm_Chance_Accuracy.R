#Function to replace data with 'NA' for subjects with overall module accuracy less than or equal to chance
#Data from each subject is individually evaluated for performance for each module (i.e. only the below chance module data will be replaced for a given subject)
#User can specify the cut_off values for each type of task and whether performance criterion should be evaluated for accuracy on the task overall or for easy conditions only
#All modules are evaluated
#Jessica Younger 11/02/2018

ACE_remove_chance = function(df, data_type, dprime_cutoff, twochoice_cutoff, fourchoice_cutoff) {

  if(data_type=="overall"){
   #Convert relevant data to numeric
    if("TNT.sdt_dprime.overall" %in% colnames(df)){ df$TNT.sdt_dprime.overall=as.numeric(df$TNT.sdt_dprime.overall)}
    if("STROOP.acc_mean.overall" %in% colnames(df)){ df$STROOP.acc_mean.overall=as.numeric(df$STROOP.acc_mean.overall)}
    if("FLANKER.acc_mean.overall" %in% colnames(df)){ df$FLANKER.acc_mean.overall=as.numeric(df$FLANKER.acc_mean.overall)}
    if("TASKSWITCH.acc_mean.overall" %in% colnames(df)){ df$TASKSWITCH.acc_mean.overall=as.numeric(df$TASKSWITCH.acc_mean.overall)}
    if("BOXED.acc_mean.overall" %in% colnames(df)){ df$BOXED.acc_mean.overall=as.numeric(df$BOXED.acc_mean.overall)}
    if("SAATIMPULSIVE.sdt_dprime.overall" %in% colnames(df)){ df$SAATIMPULSIVE.sdt_dprime.overall=as.numeric(df$SAATIMPULSIVE.sdt_dprime.overall)}
     if("SAATSUSTAINED.sdt_dprime.overall" %in% colnames(df)){ df$SAATSUSTAINED.sdt_dprime.overall=as.numeric(df$SAATSUSTAINED.sdt_dprime.overall)}
    
    #if("FILTER.k.R2B0" %in% colnames(df)){ df$FILTER.k.R2B0=as.numeric(df$FILTER.k.R2B0)}
    #if("FILTER.k.R4B0" %in% colnames(df)){ df$FILTER.k.R4B0=as.numeric(df$FILTER.k.R4B0)}
    if("FILTER.k.R2B0.weighted" %in% colnames(df)){ df$FILTER.k.R2B0.weighted=as.numeric(df$FILTER.k.R2B0.weighted)}
    if("FILTER.k.R4B0.weighted" %in% colnames(df)){ df$FILTER.k.R4B0.weighted=as.numeric(df$FILTER.k.R4B0.weighted)}
  
  if(dim(is.na(df[!is.na(df$TNT.sdt_dprime.overall) & df$TNT.sdt_dprime.overall<=dprime_cutoff, grep("TNT", names(df))]))[1]>0){is.na(df[!is.na(df$TNT.sdt_dprime.overall) & df$TNT.sdt_dprime.overall<=dprime_cutoff, grep("TNT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATIMPULSIVE.sdt_dprime.overall) & df$SAATIMPULSIVE.sdt_dprime.overall<=dprime_cutoff, grep("SAATIMPULSIVE", names(df))]))[1]>0){is.na(df[!is.na(df$SAATIMPULSIVE.sdt_dprime.overall) & df$SAATIMPULSIVE.sdt_dprime.overall<=dprime_cutoff, grep("SAATIMPULSIVE", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATSUSTAINED.sdt_dprime.overall) & df$SAATSUSTAINED.sdt_dprime.overall<=dprime_cutoff, grep("SAATSUSTAINED", names(df))]))[1]>0){is.na(df[!is.na(df$SAATSUSTAINED.sdt_dprime.overall) & df$SAATSUSTAINED.sdt_dprime.overall<=dprime_cutoff, grep("SAATSUSTAINED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$STROOP.acc_mean.overall) & df$STROOP.acc_mean.overall<=fourchoice_cutoff, grep("STROOP", names(df))]))[1]>0){is.na(df[!is.na(df$STROOP.acc_mean.overall) & df$STROOP.acc_mean.overall<=fourchoice_cutoff, grep("STROOP", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FLANKER.acc_mean.overall) & df$FLANKER.acc_mean.overall<=twochoice_cutoff, grep("FLANKER", names(df))]))[1]>0){is.na(df[!is.na(df$FLANKER.acc_mean.overall) & df$FLANKER.acc_mean.overall<=twochoice_cutoff, grep("FLANKER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$BOXED.acc_mean.overall) & df$BOXED.acc_mean.overall<=twochoice_cutoff, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.acc_mean.overall) & df$BOXED.acc_mean.overall<=twochoice_cutoff, grep("BOXED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$TASKSWITCH.acc_mean.overall) & df$TASKSWITCH.acc_mean.overall<=fourchoice_cutoff, grep("TASKSWITCH", names(df))]))[1]>0){is.na(df[!is.na(df$TASKSWITCH.acc_mean.overall) & df$TASKSWITCH.acc_mean.overall<=fourchoice_cutoff, grep("TASKSWITCH", names(df))])=TRUE}
  #if(dim(is.na(df[!is.na(df$FILTER.k.R2B0) & df$FILTER.k.R2B0<=dprime_cutoff, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R2B0) & df$FILTER.k.R2B0<dprime_cutoff, grep("FILTER", names(df))])=TRUE}
  #if(dim(is.na(df[!is.na(df$FILTER.k.R4B0) & df$FILTER.k.R4B0<=dprime_cutoff, grep("R4", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R4B0) & df$FILTER.k.R4B0<=dprime_cutoff, grep("R4", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.k.R2B0.weighted) & df$FILTER.k.R2B0.weighted<=dprime_cutoff, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R2B0.weighted) & df$FILTER.k.R2B0.weighted<=dprime_cutoff, grep("FILTER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.k.R4B0.weighted) & df$FILTER.k.R4B0.weighted<=dprime_cutoff, grep("R4", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R4B0.weighted) & df$FILTER.k.R4B0.weighted<=dprime_cutoff, grep("R4", names(df))])=TRUE}
    }

  #Convert relevant data to numeric
  if("TNT.sdt_dprime.tap_only" %in% colnames(df)){ df$TNT.sdt_dprime.tap_only=as.numeric(df$TNT.sdt_dprime.tap_only)}
  if("STROOP.acc_mean.congruent" %in% colnames(df)){ df$STROOP.acc_mean.congruent=as.numeric(df$STROOP.acc_mean.congruent)}
  if("FLANKER.acc_mean.congruent" %in% colnames(df)){ df$FLANKER.acc_mean.congruent=as.numeric(df$FLANKER.acc_mean.congruent)}
  if("TASKSWITCH.acc_mean.stay" %in% colnames(df)){ df$TASKSWITCH.acc_mean.stay=as.numeric(df$TASKSWITCH.acc_mean.stay)}
  if("BOXED.acc_mean.feature_4" %in% colnames(df)){ df$BOXED.acc_mean.feature_4=as.numeric(df$BOXED.acc_mean.feature_4)}
  if("SAATSUSTAINED.sdt_dprime.overall" %in% colnames(df)){ df$SAATSUSTAINED.sdt_dprime.overall=as.numeric(df$SAATSUSTAINED.sdt_dprime.overall)}
  if("SAATIMPULSIVE.sdt_dprime.overall" %in% colnames(df)){ df$SAATIMPULSIVE.sdt_dprime.overall=as.numeric(df$SAATIMPULSIVE.sdt_dprime.overall)}
  #if("FILTER.k.R2B0" %in% colnames(df)){ df$FILTER.k.R2B0=as.numeric(df$FILTER.k.R2B0)}
  #if("FILTER.k.R4B0" %in% colnames(df)){ df$FILTER.k.R4B0=as.numeric(df$FILTER.k.R4B0)}
  if("FILTER.k.R2B0.weighted" %in% colnames(df)){ df$FILTER.k.R2B0.weighted=as.numeric(df$FILTER.k.R2B0.weighted)}
  if("FILTER.k.R4B0.weighted" %in% colnames(df)){ df$FILTER.k.R4B0.weighted=as.numeric(df$FILTER.k.R4B0.weighted)}
  
  
  if(dim(is.na(df[!is.na(df$TNT.sdt_dprime.tap_only) & df$TNT.sdt_dprime.tap_only<=dprime_cutoff, grep("TNT", names(df))]))[1]>0){is.na(df[!is.na(df$TNT.sdt_dprime.tap_only) & df$TNT.sdt_dprime.tap_only<=dprime_cutoff, grep("TNT", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATSUSTAINED.sdt_dprime.overall) & df$SAATSUSTAINED.sdt_dprime.overall<=dprime_cutoff, grep("SAATSUSTAINED", names(df))]))[1]>0){is.na(df[!is.na(df$SAATSUSTAINED.sdt_dprime.overall) & df$SAATSUSTAINED.sdt_dprime.overall<=dprime_cutoff, grep("SUSTAINED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$SAATIMPULSIVE.sdt_dprime.overall) & df$SAATIMPULSIVE.sdt_dprime.overall<=dprime_cutoff, grep("SAATIMPULSIVE", names(df))]))[1]>0){is.na(df[!is.na(df$SAATIMPULSIVE.sdt_dprime.overall) & df$SAATIMPULSIVE.sdt_dprime.overall<=dprime_cutoff, grep("IMPULSIVE", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$STROOP.acc_mean.congruent) & df$STROOP.acc_mean.congruent<=fourchoice_cutoff, grep("STROOP", names(df))]))[1]>0){is.na(df[!is.na(df$STROOP.acc_mean.congruent) & df$STROOP.acc_mean.congruent<=fourchoice_cutoff, grep("STROOP", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FLANKER.acc_mean.congruent) & df$FLANKER.acc_mean.congruent<=twochoice_cutoff, grep("FLANKER", names(df))]))[1]>0){is.na(df[!is.na(df$FLANKER.acc_mean.congruent) & df$FLANKER.acc_mean.congruent<=twochoice_cutoff, grep("FLANKER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$BOXED.acc_mean.feature_4) & df$BOXED.acc_mean.feature_4<=twochoice_cutoff, grep("BOXED", names(df))]))[1]>0){is.na(df[!is.na(df$BOXED.acc_mean.feature_4) & df$BOXED.acc_mean.feature_4<=twochoice_cutoff, grep("BOXED", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$TASKSWITCH.acc_mean.stay) & df$TASKSWITCH.acc_mean.stay<=fourchoice_cutoff, grep("TASKSWITCH", names(df))]))[1]>0){is.na(df[!is.na(df$TASKSWITCH.acc_mean.stay) & df$TASKSWITCH.acc_mean.stay<=fourchoice_cutoff, grep("TASKSWITCH", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.k.R2B0.weighted) & df$FILTER.k.R2B0.weighted<=dprime_cutoff, grep("FILTER", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R2B0.weighted) & df$FILTER.k.R2B0.weighted<dprime_cutoff, grep("FILTER", names(df))])=TRUE}
  if(dim(is.na(df[!is.na(df$FILTER.k.R4B0.weighted) & df$FILTER.k.R4B0.weighted<=dprime_cutoff, grep("R4", names(df))]))[1]>0){is.na(df[!is.na(df$FILTER.k.R4B0.weighted) & df$FILTER.k.R4B0.weighted<=dprime_cutoff, grep("R4", names(df))])=TRUE}
  return(df)}
