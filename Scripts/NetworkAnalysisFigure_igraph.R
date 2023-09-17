#Script to generate the Figure of Network Analysis and Community Detection Results (Figure 2)
#Customized script is used to generate figures for publication that improve readability of the final results derived from CommunityDetectionwithIterations.R script
#Jessica Younger 2023-06-30

# Clear environment
rm(list = ls())
#Library the packages used in this script
library(tidyverse)
library(bootnet)
library(qgraph)
library(igraph)

#Set the working directory to the folder with data and functions for this project
setwd("~/path/to/working/directory")

#Source the functions used in this script
source("Functions/SpinGlassCommunityDetectionIterations_Function.R")

#Load the data - output of projectiLEAD_triallevelprocessing.R Script
data=read.csv(file="Data/proj-InitialPapers_asmt-A_time-1234_type-agg_shape-long_clean-trimRTAccExOutlierRmBRTRegress_prog-R_date-2023-09-09.csv")

#Select the subset of data needed for the community detection analysis 
names(data)
data=data%>%
  select(pid:grade,BACKWARDSSPATIALSPAN.object_count_span.overall.residuals:STROOP.rcs.overall.residuals, BOXED.rt_mean.correct.residuals )

# Rename columns for the metrics of interest to make the results easier to read
colnames(data) = c(names(data)[1:4], 'BSS', 'FSS', 'SSUS',
                   'SIMP', 'TNT', 'FLNK', 'STRP', 'BOX')

#Order the data for easier checking
data = data[order(data$pid, data$Time.Point), ]

#Remove data sets that are missing all ACE module data (i.e., number os missing data sets is less than 8, the total nubmer of modules used)
data=
  data%>%    
  mutate(na_count= apply(., 1, function(x) sum(is.na(x))))%>%
  group_by(Cohort, na_count)%>%
  filter(na_count<8)

#Recode the modules with RT related metrics so they go in the same direction as the oter modules
data$BOX=-1*data$BOX
data$SSUS=-1*data$SSUS

#Set up the label names for the output
colnames(data) = c(names(data)[1:4], "B. Span", "F. Span", "Sust\nAttn", "Impul\n Attn", 
                   "Tap &\n Trace", "Flanker", "Stroop", "Boxed")

#Set the colors to be used for the graph
three_factor=c("#d2d6cd","#faec2a", "#3ad683")
colors_c3t1 = c("#d2d6cd", "#ffffff")
colors_c3t2 = c("#d9e68c", "#3ad683")
colors_c3t3 = c("#faec2a", "#3ad683")
colors_c3t4 = c("#a3d59d","#faec2a")


background_3=c("#ebebeb", "#f7fa99", "#98eb9b")
background_c3t1 = c("#ebebeb", "#ffffff")
background_c3t4=c("#c4ecc3", "#f7fa99")
background_c3t3=c("#f7fa99", "#98eb9b")
background_c3t2=c("#f3f2c3", "#98eb9b")


#Set up the labels to be used
labels=c("B. Span", "F. Span", "Sust\nAttn", "Impul\n Attn", 
         "Tap &\n Trace", "Flanker", "Stroop", "Boxed")

#Run the network analyses
#For each cohort
for (i in c(3,5,7)){
  #For each timepoint
  for (j in c(1,2,3,4)){
    #run the network analyses
    network=estimateNetwork(data[data$Cohort==i & data$Time.Point==j, c(5:12)],default="pcor", corMethod="cor_auto", missing="fiml")
    #plot
    g_net=as.igraph(plot(network, layout="circle", labels=colnames(network), label.cex=3))
    #set seed for future replication 
    set.seed(5)
    #get a community structure
    g=cluster_spinglass(g_net)
    V(g_net)$community <- g$membership
    g_net$layout=layout_in_circle(g_net)
    #Scale the edgeweights so that only strength of connections is more visible
    E(g_net)$weight=ifelse(E(g_net)$weight>-0.1 & E(g_net)$weight<0.1,0, E(g_net)$weight)
    E(g_net)$lty <- ifelse(E(g_net)$weight>0, 1, ifelse(E(g_net)$weight==0, 0, 2))
    E(g_net)$width <- abs(E(g_net)$weight*15)
    #Save the results to separate variables
    assign(paste0("network_c", i, "t", j), g_net)
    assign(paste0("groups_c", i, "t", j), g)
  }
}
#This code overwrites the membership determined in the initial network graph to 
#a) correspond to the results of the 1000 iterations of community detection and 
#b) to get consistency of the community colors across networks
groups_c3t1$membership=c(1,1,2,2,2,2,2,2)
groups_c3t2$membership=c(1,1,1,1,1,2,2,2)
groups_c3t3$membership=c(1,2,1,1,1,2,2,2)
groups_c3t4$membership=c(1,1,2,2,2,1,1,1)
groups_c5t1$membership=c(1,1,3,2,2,2,3,3)
groups_c5t2$membership=c(1,1,2,2,2,3,3,3)
groups_c5t3$membership=c(1,1,2,2,2,3,3,3)
groups_c5t4$membership=c(1,1,2,2,2,3,3,3)
groups_c7t1$membership=c(1,1,2,2,2,3,3,3)
groups_c7t2$membership=c(1,1,2,2,3,3,3,3)
groups_c7t3$membership=c(1,1,2,2,2,3,3,3)
groups_c7t4$membership=c(1,1,2,2,2,3,3,3)


#Create a dummy network to pull values for the legend
edge_list <- tibble(from = c(1, 2, 3, 4), to = c(2, 4, 2, 1))
node_list <- tibble(id = 1:4)
network_dummy <- graph_from_data_frame(d = edge_list, vertices = node_list, directed = TRUE)
E(network_dummy)$weight=c(0.1, -0.2, 0.3, 0.4)
E(network_dummy)$width=abs(E(network_dummy)$weight*15)
E(network_dummy)$lty <- ifelse(E(network_dummy)$weight>0, 1, ifelse(E(network_dummy)$weight==0, 0, 2))
lwd <- E(network_dummy)$width

#Set up to save the plot
path=paste0("Results/Figure2_NetworkAnalysis_Communities_", Sys.Date(), ".jpeg")
jpeg(file=path, units="in", width=10, height=10, res=300)

#Set up the spacing for the graph
par(mfrow=c(3,4),oma=c(15.5,0,0,0), xpd=NA)
par(mai=c(0.3,0.5,0.5,0.3))
#Plot the results for  each cohort and set up labels for Timepoint 
#Cohort 3
plot(groups_c3t1, network_c3t1,  vertex.size=45, vertex.label.cex=1, palette=colors_c3t1, mark.border="black", mark.col=background_c3t1,
      vertex.label=labels,  rescale=F)
title(ylab="3rd - 4th Grade Cohort", line = 2, outer=F, cex=4)
title(main="Timepoint 1",line=1)
title(xlab = paste0("N = ", length(data$Cohort[data$Cohort==3 & data$Time.Point==1])), line = 1, outer=F, cex=4)

plot(groups_c3t2, network_c3t2, vertex.size=45, vertex.label.cex=1, rescale=F, palette=colors_c3t2, mark.border="black", mark.col=background_c3t2,
     vertex.label=labels)
title(main="Timepoint 2",line=1)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==3 & data$Time.Point==2])),line = 1, outer=F, cex=4)


plot(groups_c3t3, network_c3t3, vertex.size=45, vertex.label.cex=1, rescale=F,palette=colors_c3t3, mark.border="black", mark.col=background_c3t3,
     vertex.label=labels)
title(main="Timepoint 3",line=1)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==3 & data$Time.Point==3])),line = 1, outer=F, cex=4)


plot(groups_c3t4, network_c3t4, vertex.size=45, vertex.label.cex=1, rescale=F,palette=colors_c3t4, mark.border="black", mark.col=background_c3t4,
     vertex.label=labels )
title(main="Timepoint 4",line=1)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==3 & data$Time.Point==4])),line = 1, outer=F, cex=4)


#Cohort 5
plot(groups_c5t1, network_c5t1, vertex.size=45, vertex.label.cex=1, rescale=F,palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(ylab="5th - 6th Grade Cohort", line = 2, outer=F, cex=4)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==5 & data$Time.Point==1])),line = 1, outer=F, cex=4)


#lines(x=5)
plot(groups_c5t2, network_c5t2, vertex.size=45, vertex.label.cex=1, rescale=F,palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==5 & data$Time.Point==2])),line = 1, outer=F, cex=4)

plot(groups_c5t3, network_c5t3, vertex.size=45, vertex.label.cex=1, rescale=F,palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==5 & data$Time.Point==3])),line = 1, outer=F, cex=4)

plot(groups_c5t4, network_c5t4,vertex.size=45, vertex.label.cex=1, rescale=F, palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(sub = paste0("N = ", length(data$Cohort[data$Cohort==5 & data$Time.Point==4])),line = 1, outer=F, cex=4)

#Cohort 7
plot(groups_c7t1, network_c7t1, vertex.size=45, vertex.label.cex=1, rescale=F,palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(ylab="7th - 8th Grade Cohort", line = 2, outer=F, cex=4)
title(xlab = paste0("N = ", length(data$Cohort[data$Cohort==7 & data$Time.Point==1])),line = 1, outer=F, cex=4)

plot(groups_c7t2, network_c7t2,vertex.size=45, vertex.label.cex=1, rescale=F, palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(xlab = paste0("N = ", length(data$Cohort[data$Cohort==7 & data$Time.Point==2])),line = 1, outer=F, cex=4)

plot(groups_c7t3, network_c7t3,vertex.size=45, vertex.label.cex=1, rescale=F, palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(xlab = paste0("N = ", length(data$Cohort[data$Cohort==7 & data$Time.Point==3])),line = 1, outer=F, cex=4)

plot(groups_c7t4, network_c7t4, vertex.size=45, vertex.label.cex=1, rescale=F,palette=three_factor, mark.border="black", mark.col=background_3,
     vertex.label=labels)
title(xlab = paste0("N = ", length(data$Cohort[data$Cohort==7 & data$Time.Point==4])),line = 1, outer=F, cex=4)


#Set up the legend for color
legend(-7.5, -2,   ## position, also takes x,y coordinates
  legend = c("CM","CM + WM",  "WM",  "IR +WM","IR", "CM+IR"),
  #pch = 22,              ## legend symbols see ?points
  fill =c("#faec2a","#d9e68c", "#d2d6cd", "#a3d59d","#3ad683", "#ffffff"),
  border = "black",
  bty = "n",
  cex=1.5,
  pt.cex = 50,
 # lwd=4,
  title = "Node Color",
  title.adj = 0, 
  xpd=NA)

#Set up the legend for edge weight line thickness
legend(-5.5,-2,      ## position, also takes x,y coordinates
       legend = abs((E(network_dummy))$weight),
       lwd=lwd,# pch = 19,              ## legend symbols see ?points
       # col =c("#faec2a","#d9e68c", "#d2d6cd", "#a3d59d","#3ad683"),
       bty = "n",
       title = "Edge Weight",
       cex=1.5,
       title.adj = 0, 
       xpd=NA)

#Set up the legend for line type
legend(-3.5,-2,     ## position, also takes x,y coordinates
       legend = c("Positive", "Negative"),
       lty=E(network_dummy)$lty,# pch = 19,              ## legend symbols see ?points
       # col =c("#faec2a","#d9e68c", "#d2d6cd", "#a3d59d","#3ad683"),
       bty = "n",
       title = "Edge Type",
       cex=1.5,
       title.adj = 0, 
       xpd=NA)

#Set up the legend for line color
legend(-3.5,-3,     ## position, also takes x,y coordinates
       legend = c("Within Community", "Between Community"),
       lwd=5,
       col =c("black", "red"),
       bty = "n",
       title = "Edge Color",
     cex=1.5,
     title.adj = 0, 
       xpd=NA)

dev.off()  


 





