#Data structure
#Dominik Bahlburg
#May 23rd, 2017
library(ggplot2)
library(readr)
library(vegan)
library(dplyr)
#read in data
data_tidy <- read_csv("~/Roundtable/Data Structure 23.5.2017/dummy_data.csv")
data_matrix <- read_csv("~/Roundtable/Data Structure 23.5.2017/dummy_data_matrix.csv")
str(data_tidy)
str(data_matrix)

#data_matrix format for matrix-based operations:
#Maybe we want to calculate distance measures, make an nmds plot or whatever:
#Then data_matrix would be the format to go with (each observed community is one row)
MDS_data <- metaMDS(data_matrix[,4:9], k=2, autotransform = TRUE)
scores_MDS <- as.data.frame(scores(MDS_data)) 
data_scores <- cbind.data.frame(scores_MDS,data_matrix[,1:3]) 
nmds_data<-ggplot() + 
  geom_point(data=data_scores,aes(x=NMDS1,y=NMDS2,shape=Country,colour=Area),size=3) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
nmds_data

#However, very often we don't work with multivariate methods and want to summarize
#data, build models... This is when tidy data come into play
attach()
means_community <- summarise(group_by(data_tidy, country, area, species), mean_specs = mean(count), sd_specs = sd(count))
ggplot(means_community, aes(x = area, y = mean_specs, colour = species, shape = country))+
  geom_point() 
#  geom_errorbar(ymin = means_community$mean_specs - means_community$sd_specs, ymax = means_community$mean_specs + means_community$sd_specs)
aov_country <- aov(count~country+species, data = data_tidy)
summary(aov_country)
TukeyHSD(aov_country)
