#Function to set up and process trial level data to averaged data. 
#User specifies the raw data set, the cutoff_min value (standard for iLEAD is 200), and the names of the metrics of interest that should be output in the final dataframe
#Trial level data is prepped for processing, trials < cutoff min are removed, then processed. The DF is reduced to only output the columns with the metrics of interest for each module
#Additionally, there is code to  calculate weighted K for FILTER and then merge into the main data frame
#Jessica Younger 11/22/22

ACE_process_trialleveldata = function(df, cutoff_min, metric_names, late_incorrect) {
  library(tidyverse)
  library(aceR)
#proc_by_module only works on character data frames, so if you read in from a csv file, convert all data to character strings (instead of numeric, factors, etc)
   df[] <- lapply(df, as.character)
#An edit was made to how aceR processes SAAT data and this code modifies the module name to make these data compatible with updated code
     df <- df %>%
    mutate(module = if_else(module == "SAAT", paste0(module, toupper(condition)), module))
#the proc_by_module script only works on nested data, so run this section to prepare for processing
      df_nest=nest_ace_raw(df, app_type="classroom")
#Specify minimum usable RT of 200ms
  df_nest=trim_rt_trials_range(df_nest, cutoff_min = cutoff_min)
#do processing on data, assign to a variable. 
  df_avg=proc_by_module(df_nest, app_type="classroom", verbose=TRUE)
#Reducing the columns to metrics of interest and get rid of labels no longer in use
  df_avg=df_avg%>%
    post_reduce_cols(demo_names = c("pid", "age", "grade", "gender", "handedness"), metric_names =  metric_names)%>%
    select(-contains("early"), -contains("median"), -contains("SPATIALSPAN.rt_"), -contains("start"), -contains("cost"), -contains("late"))%>%
    droplevels()
#Need to run a script to calculate weighted K and then merge into the main data frame
  df_k=k_calc(df)
#put the two data frames togehter
  df_all=merge(df_avg, df_k,  by=c("pid"), all=TRUE)
  df_all
}
