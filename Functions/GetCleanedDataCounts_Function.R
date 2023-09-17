##Function to get the total number of data sets available after each major stage of cleaning. 
#Requires the dataframes generated as intermediate stages in the projectiLEAD_triallevelprocessing.R script
#Jessica Younger 11-22-2022

ACE_datasetscleaning = function(variables=c()) {
  #set up the empty data frame to capture results
nremoved=data.frame(task=as.character(), total=as.numeric(), lowcount=as.numeric(), belowchance=as.numeric(), outlier=as.numeric(), cooksdoutlier=as.numeric(),stringsAsFactors=F)
#for each variable input, count the number of datasets that are not NA, and put into the nremoved dataframe
for (i in 1:length(variables)){
nremoved[i,1]=variables[i]
#colorblind
nremoved[i,2]=
  data%>%
  select(variables[i])%>%
  na.omit()%>%
  count()
#low count
nremoved[i,3]=
dataclean%>%
  select(variables[i])%>%
  na.omit()%>%
  count()
#chance
nremoved[i,4]=
data_chance%>%
  select(variables[i])%>%
  na.omit()%>%
  count()
#mad outlieres
nremoved[i,5]=
data_clean%>%
  select(variables[i])%>%
  na.omit()%>%
  count()
#cooks d outliers
nremoved[i,6]=
compiled%>%
  select(variables[i])%>%
  na.omit()%>%
  count()
}
nremoved
}

