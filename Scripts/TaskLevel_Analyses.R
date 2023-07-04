##This script is used to get the results of the linear models examining the effect of time and age on performance for each ACE task
#Also generates the resulting figures related to the linear model results (Figure 1 and Figure S4)
#Jessica Younger 2023-06-30


#Library the packages used in this script
library(tidyverse)
library(lme4)
library(lmerTest)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(patchwork)


#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-AS_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-03-07.csv")

#Ensure the data are set as the correct variable type (factor vs numeric etc)
str(data)
data$Time.Point=as.numeric(data$Time.Point)
data$Cohort=as.factor(data$Cohort)
data$Language.Fluency=as.factor(data$Language.Fluency)
data$time_since_firstgameplay=as.numeric(data$time_since_firstgameplay)
data$age_months=as.numeric(data$age_months)
data$gender=as.factor(data$gender)

#Transform the RT related variables so they are on a similar scale and direction as other variables (so higher score is better)
data$SAATSUSTAINED.rt_sd.correct=-1*data$SAATSUSTAINED.rt_sd.correct/100
data$BOXED.rt_mean.correct=-1*data$BOXED.rt_mean.correct/100

#####Calculate Results of Linear Models#####
#Main effects with only data available for all participants (BRT, gender, Cohort, School, no district data)
#Can be modified to include additional covariates/interactions with various other data

#Gather the column names of the metrics that will be analyzed
colnames=
  c("SAATSUSTAINED.rt_sd.correct",
    "SAATIMPULSIVE.sdt_dprime.overall",
    "TNT.sdt_dprime.tap_trace",
    "FLANKER.rcs.overall",
    "STROOP.rcs.overall",
    "BOXED.rt_mean.correct", 
    "SPATIALSPAN.object_count_span.overall", 
    "BACKWARDSSPATIALSPAN.object_count_span.overall")

#Set up to write out the results to a text file
sink(paste0("Results/TasklevelAnalysis_LinearModels_MainEffectResults_", Sys.Date(), ".txt"))
#Run the linear model
for(i in colnames){
  a=lmer(data[[i]]~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
  print(i)
  print(summary(a))
  print(anova(a, type=3))
}
#Finish writing to text file
sink()



####Plot the Effects of each (Figure S3)####
#Save the results of the Linear Model for each task to its own variable for plotting
sus=lmer(SAATSUSTAINED.rt_sd.correct~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
imp=lmer(SAATIMPULSIVE.sdt_dprime.overall~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
tnt=lmer(TNT.sdt_dprime.tap_trace~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
flnk=lmer(FLANKER.rcs.overall~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
stroop=lmer(STROOP.rcs.overall~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
box=lmer(BOXED.rt_mean.correct~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
fss=lmer(SPATIALSPAN.object_count_span.overall~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 
bss=lmer(BACKWARDSSPATIALSPAN.object_count_span.overall~ time_since_firstgameplay * age_months + BRT.rt_mean.correct + Cohort +  gender + (1 + time_since_firstgameplay|pid) + (1|School), data=data) 


#Set the Theme
theme_set(theme_classic())

#Set up the plots for the Context Monitoring Tasks
a=plot_models(tnt, imp,sus,axis.labels=c("Time* Age", "Gender", "Cohort: 7th - 8th Grade", "Cohort: 5th - 6th Grade", "BRT (ms)", "Age (months)", "Time Since First Task Engagement (months)"),
            m.labels = c( "Tap and Trace (d')\nN=2,745","Impulsive Attention (d')\nN=3,525", "Sustained Attention (RT sd)\nN=3,494"), dot.size=3,
            show.values = FALSE, show.p = TRUE, p.shape = FALSE, std.est=TRUE,  axis.title="Estimate",
            colors=c("#f1e249", "#f1e249", "#f1e249"), wrap.labels = 30)
RI=a+geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), color="gray97", linetype="F1", size=0.5)+
aes(shape=group) +guides(color=FALSE)+scale_y_continuous(limits = c(-.5, .5))+theme(text = element_text(size=16), axis.text=element_text(colour="black"), legend.position = "bottom",plot.title = element_text(hjust=0),legend.text = element_text(hjust=0.5, vjust=0.5))+
  labs(shape="", title="Context Monitoring")+   geom_text_repel(aes(label=a$data$p.label), color="black",  
                                                          position = position_dodge(0.3)) 
                                                    

#Set up the plots for the Interference Resolution Tasks
b=plot_models(box,stroop,  flnk, axis.labels=c("Time* Age", "Gender", "Cohort: 7th - 8th Grade", "Cohort: 5th - 6th Grade", "BRT (ms)", "Age (months)", "Time Since First Task Engagement (months)"),
             m.labels = c("Boxed (mean RT)\nN=3,368","Stroop (RCS)\nN=3,497","Flanker (RCS)\nN=3,470"), dot.size=3,
              show.values = FALSE, show.p = TRUE, p.shape = FALSE, std.est=TRUE, spacing=0.5,axis.title="Estimate",
              colors=c("#30c16a", "#30c16a", "#30c16a"))
IR=b+geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), color="gray97", linetype="F1", size=0.5)+scale_alpha(range = c(1,1))+
aes(shape=group) +guides(color=FALSE)+scale_y_continuous(limits = c(-.5, .5))+theme(text = element_text(size=16), axis.text=element_text(colour="black"), legend.position = "bottom",plot.title = element_text(hjust=0), axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_blank(),legend.text = element_text(hjust=0.5, vjust=0.5) )+
  labs(shape="", title="Interference Resolution")+  geom_text_repel(aes(label=b$data$p.label), color="black",  
                                                                          position = position_dodge(0.3)) 

#Set up the plots for the Working Memory Tasks
c=plot_models(bss,fss,  axis.labels=c("Time* Age", "Gender", "Cohort: 7th - 8th Grade", "Cohort: 5th - 6th Grade", "BRT (ms)", "Age (months)", "Time Since First Task Engagement (months)"),
              m.labels = c( "Backwards Span (Span)\nN=3,522","Forward Span (Span)\nN=3,522"), dot.size=3,
              show.values = FALSE, show.p = TRUE, p.shape = FALSE, std.est=TRUE, spacing=0.5,axis.title="Estimate",
              colors=c("#a4a8a0", "#a4a8a0"))
WM=c+geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), color="gray97", linetype="F1", size=0.5)+scale_alpha(range = c(1,1))+
aes(shape=group) +guides(color=FALSE)+scale_y_continuous(limits = c(-.5, .5))+theme(text = element_text(size=16), axis.text=element_text(colour="black"), legend.position = "bottom", plot.title = element_text(hjust=0), axis.text.y = element_blank(), axis.ticks.y = element_blank(),axis.title.y = element_blank(),legend.text = element_text(hjust=0.5, vjust=0.5))+
  labs(shape="", title="Working Memory")+ geom_text_repel(aes(label=c$data$p.label), color="black",  
                                                          position = position_dodge(0.3)) 
  

#Combine the three plots into one
List <- list(RI, IR, WM)
arranged <- wrap_plots(List,ncol = 3,nrow = 1)
#Save as a file
out_path <- file.path(paste0("Results/", "FigureS4_TaskLevelAnalyses_LinearModel_FixedEffectEstimates_", Sys.Date(), ".png"))
png(file=out_path, units="in", width=24, height=10, res=300)
plot(arranged)
dev.off()  


####Create the Line Plots (Figure 1)

#Set the Levels of Cohort to the formal name
levels(data$Cohort)=c("3rd-4th grade", "5th-6th grade", "7th-8th grade")
#Put the RT related variables back on their regular scale for visualization
data$SAATSUSTAINED.rt_sd.correct=-1*data$SAATSUSTAINED.rt_sd.correct*100
data$BOXED.rt_mean.correct=-1*data$BOXED.rt_mean.correct*100

#Create the individual plots for each task and save to a variable
fss=
  data%>% 
  ggplot(aes_string(y="SPATIALSPAN.object_count_span.overall")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(4,7))+
  labs(x="Time (months)", y="Span", title="Forward Spatial Span") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12))

bss=
  data%>% 
  ggplot(aes_string(y="BACKWARDSSPATIALSPAN.object_count_span.overall")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(4,7))+
  labs(x="Time (months)", y="Span", title="Backward Spatial Span") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

st_sus=
  data%>% 
  ggplot(aes_string(y="SAATSUSTAINED.rt_sd.correct")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(70,100))+
  labs(x="Time (months)", y="Standard Deviation of Reaction Time", title="Sustained Attention") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

st_imp=
data%>% 
  ggplot(aes_string(y="SAATIMPULSIVE.sdt_dprime.overall")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(1,3.5))+
  labs(x="Time (months)", y="d'", title="Impulsive Attention") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 
tnt=
data%>% 
  ggplot(aes_string(y="TNT.sdt_dprime.tap_trace")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(1,3.5))+
  labs(x="Time (months)", y="d'", title="Tap and Trace") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

flnk=
  data%>% 
  ggplot(aes_string(y="FLANKER.rcs.overall")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(1,2))+
  labs(x="Time (months)", y="Rate Correct Score", title="Flanker") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

strp=
  data%>% 
  ggplot(aes_string(y="STROOP.rcs.overall")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(1,2))+
  labs(x="Time (months)", y="Rate Correct Score", title="Stroop") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),legend.position = "none",
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

box=
  data%>% 
  ggplot(aes_string(y="BOXED.rt_mean.correct")) +
  geom_smooth(aes(x=time_since_firstgameplay, color=Cohort, group=Cohort, fill=Cohort),method="lm", alpha=0.2)+
  facet_wrap(~Cohort, ncol=4)+
  coord_cartesian(ylim=c(600,900))+
  labs(x="Time (months)", y="Reaction Time", title="Boxed (visual search)") +
  scale_color_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  scale_fill_manual(name="Cohort", values=c("#7bccc4", "#43a2ca", "#0868ac")) +
  theme_classic()+
  theme(text = element_text(size=14))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(size=12,margin=margin(0,18,0,0)),
        axis.title.x=element_text(size=12,margin=margin(18,0,0,0)), text=element_text(size=12)) 

#Get the legend from one of the plots
legend=get_legend(box)

#Then turn it off for plotting
box=box+ theme(legend.position = "none")

#Arrange the individual plots into one big one
arranged=
  ggarrange(st_sus, st_imp, tnt, strp, flnk, box, bss, fss, legend,
            ncol = 3, nrow = 3)

#Save it to a file
out_path <- file.path(paste0("Results/Figure1_TaskLevelAnalysis_LinearModelResultsplot_", Sys.Date(), ".jpeg", sep = ""))
jpeg(file=out_path, units="in", width=12, height=12, res=300)
plot(arranged)
dev.off()  
