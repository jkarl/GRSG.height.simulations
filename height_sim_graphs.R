## height_sim_graphs.R
## Code to construct the simulation graphs from DiStefano et al. (2018)
## Requires that the simulations have already been run and output as .CSV files

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(reshape2)

## Set the working directory to where the simulation CSV files are stored.
## Alternatively, remove this statement and provide full pathways to the files below.
setwd("C:\\Users\\Jason Karl\\Documents\\GitHub\\GRSG.height.simulations\\data")

#load data from the different simulations and append into a single data frame
infile <- "aim-haf_compare_d50_medium.csv"
data <- read.csv(infile,header=T,stringsAsFactors = F)
infile <- "aim-haf_compare_d100_small.csv"
data <- rbind(data,read.csv(infile,header=T,stringsAsFactors = F))

## Set up the facets for graphing
data$facets <- paste(data$num.plants,data$plant.size,sep="-")

## Calc the summary statistics
data <- data %>% mutate(aim.actual.avg=(aim.sum-plot.average), aim.actual.max=(aim.sum-plot.max), haf.actual.avg=(haf.sum-plot.average), haf.actual.max=(haf.sum-plot.max))

## Filter to only those with HAF measurements
data.filter <- data[data$haf.sum>0,]

## Set up pretty names
facet_names <- c("50-25"="Large, Sparse","100-10"="Small, Dense")

## AIM vs. HAF height measurements
ggplot(data.filter, aes(x=aim.sum,y=haf.sum))+geom_point()+
  geom_abline(slope=1,intercept=0,color="black",linetype="longdash")+
  geom_smooth(method="lm",se=F,color="black",linetype="solid")+
  stat_poly_eq(formula=y~x,aes(label=paste(..eq.label.., ..rr.label.., sep="~~~")),parse=T, label.x.npc = "right", label.y.npc = "bottom")+
  facet_grid(~facets,labeller=labeller(facets=facet_names))+
  xlab("Plot Average Simulated AIM Height (cm)")+ylab("Plot Average Simulated HAF Height (cm)")
ggsave(filename = "Simulated_AIM-v-HAF_heights.tif", device = "tiff")

## AIM and HAF height measurements vs. plot actual
vs.actual <- data.filter %>%  melt(id.vars="facets",measure.vars=c("aim.actual.avg","aim.actual.max","haf.actual.avg","haf.actual.max"))
vs.actual$method <- paste(substr(vs.actual$variable,12,14),substr(vs.actual$variable,1,3),sep="-")
ggplot(vs.actual, aes(method,value))+geom_boxplot()+
  geom_hline(yintercept=0)+
  geom_jitter(width=0.1)+
  facet_grid(~facets,labeller=labeller(facets=facet_names))
ggsave(filename = "Simulation_diff_from_actual.tif", device = "tiff")
