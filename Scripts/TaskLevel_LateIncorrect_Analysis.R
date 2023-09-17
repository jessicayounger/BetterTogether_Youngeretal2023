##This script is used to assess in-game mean accuracy experienced by participants to understand the average percentage of trials with 'correct' feedback received across cohorts for modules with an adaptive response window
#Used to generate Table 2 and Figure S5in the main manuscript
#Jessica Younger 2023-06-30


#Library the packages used in this script

library(tidyverse)
library(lme4)
library(lmerTest)
library(sjstats)
library(car)
library(r2glmm)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(patchwork)

#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the data - special modified data frame in which all 'late' trials were considered incorrect
data=read.csv(file="Data/proj-InitialPapers_asmt-A_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmLateIncorrect_prog-R_date-2023-09-09.csv")


#Print the values for mean and sd of accuracy for each module by Cohort
sink(paste0("Results/TaskLevelAnalysis_LateIncorrect_SummaryStats_cohorttime_mean_sd_", Sys.Date(), ".txt"))
data%>%
  group_by(Cohort, Time.Point)%>%
  summarize_at(c(
    'SAATSUSTAINED.acc_mean.overall',
    "SAATIMPULSIVE.acc_mean.overall",
    "TNT.acc_mean.tap_trace",
    "FLANKER.acc_mean.overall",
    "STROOP.acc_mean.overall",
    "BOXED.acc_mean.overall"),  list(mean = mean, sd = sd), na.rm=TRUE) %>%
  print(width=Inf)
sink()

#Gather the column names of the metrics that will be analyzed
colnames=
  c("SAATSUSTAINED.acc_mean.overall",
    "SAATIMPULSIVE.acc_mean.overall",
    "TNT.acc_mean.overall",
    "STROOP.acc_mean.overall",
    "FLANKER.acc_mean.overall",
    "BOXED.acc_mean.overall")

#Set up to write out the results to a text file
sink(paste0("Results/TaskLevelAnalysis_LateIncorrect_LinearModelResults_", Sys.Date(), ".txt"))
#Run the linear model and get model effect sizes. 
#Note the results of the anova are to evaluate significance. F values and DF are pulled from the results of the r2beta function
for (i in colnames){
a=lmer(data[[i]]~ Cohort*Time.Point+ (1|pid) + (1|School), data=data)
b=r2beta(a, method="nsj", partial=FALSE)
print(i)
print(anova(a, type=3))
print (paste0("Model F value:", round(b$F, 2)))
print(paste0("NumDF:", b$v1))
print(paste0("DenDF:", b$v2))
print(b)
}
sink()


####Create the Line Plots (Figure 1)

#Set the Levels of Cohort to the formal name
data$Cohort=as.factor(data$Cohort)
levels(data$Cohort)=c("3rd-4th grade", "5th-6th grade", "7th-8th grade")

#Create the individual plots for each task and save to a variable
st_sus=
  data%>% 
  ggplot(aes_string(y="SAATSUSTAINED.acc_mean.overall")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Sustained Attention") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

st_imp=
  data%>% 
  ggplot(aes_string(y="SAATIMPULSIVE.acc_mean.overall")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Impulsive Attention") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

tnt=
  data%>% 
  ggplot(aes_string(y="TNT.acc_mean.tap_trace")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Tap and Trace") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

flnk=
  data%>% 
  ggplot(aes_string(y="FLANKER.acc_mean.overall")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Flanker") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

strp=
  data%>% 
  ggplot(aes_string(y="STROOP.acc_mean.overall")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Stroop") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

box=
  data%>% 
  ggplot(aes_string(y="BOXED.acc_mean.overall")) +
  geom_boxplot(aes(x=as.factor(Time.Point), fill=Cohort))+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Timepoint", y="Accuracy", title="Boxed (visual search)") +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),#legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

#Get the legend from one of the plots
legend=get_legend(box)

#Then turn it off for plotting
box=box+ theme(legend.position = "none")

#Arrange the individual plots into one big one
arranged=
  ggarrange(st_sus, st_imp, tnt, strp, flnk, box,
            common.legend = TRUE,legend='right',
            ncol = 3, nrow = 2)

#Save it to a file
out_path <- file.path(paste0("Results/FigureS5_TaskLevelAnalysis_LateIncorrect_boxplots_", Sys.Date(), ".jpeg", sep = ""))
jpeg(file=out_path, units="in", width=14, height=8, res=300)
plot(arranged)
dev.off()  

