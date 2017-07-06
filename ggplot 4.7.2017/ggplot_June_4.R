rm(list=ls(all=T))

library(ggplot2)
library(plyr)
library(grid)
library(scales)

setwd("C:/Users/planktologie/Dropbox/PHD/Versuch 2014/R")
data <- read.table("Abs_Andrea.txt",sep="\t",header=TRUE)
data<-subset(data, Day >50)
data$DOM<-as.factor(data$DOM)
data$Daphnia<-as.factor(data$Daphnia)

head(data)

#####basic graph#####
ggplot(data,aes(Day, DecAbsCoeff440))+
  geom_point(size=2)


#####differentiation between parameters with shapes#####
ggplot(data,aes(Day, DecAbsCoeff440,shape = DOM))+
  geom_point(size=2)+
  scale_shape_manual(values= c(1,2),labels = c("- DOM", "+ DOM"))
      ##### values: 0=square; 1=circle; 2=triangle; 15=square filled; 16=circle       filled; 17=triangle filled
            

#####differentiation between parameters with colour#####
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))
  #scale_colour_manual(values= c('#FF0000','#0000FF'),labels = c("- DOM", "+ DOM")) #http://www.rapidtables.com/web/color/RGB_Color.htm -> calculates the hex code into decimal code (rgb) same colours in r and powerpoint
 
    
  
#####colour & shape together#####
ggplot(data,aes(Day, DecAbsCoeff440,shape=Daphnia,colour = DOM))+
  geom_point(size=2)+
  scale_shape_manual(values= c(15,16),labels = c("- Daphnia", "+ Daphnia"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM")) 


#####colour & line together#####
library(RColorBrewer)
display.brewer.all(n=10)
brewer.pal.info
display.brewer.pal(9,"Set1")
brewer.pal(9,"Set1")


mean440<-ddply(data,c("Day","DOM"), summarise,
               Mean_Chl=mean(DecAbsCoeff440, na.rm=TRUE),
               sd = sd(DecAbsCoeff440, na.rm=TRUE),
               n = sum(!is.na(DecAbsCoeff440)),
               se = sd/sqrt(n))

ggplot(mean440,aes(Day, Mean_Chl, shape=DOM, colour=DOM))+
  scale_fill_brewer(palette="Set1")+
  geom_point(aes(fill=DOM), colour="black", shape=21, size=6)+
  geom_line(aes(colour=DOM), size=1)+
  scale_color_manual(values=c("#FF7F00","#F781BF"))

#### adjustes the position of lines/bargraphs/points to left or right#####
pd <- position_dodge(.9)
ggplot(data,aes(Day, DecAbsCoeff440,shape=DOM,colour = DOM))+
  geom_point(size=2,position=pd)+
  scale_shape_manual(values= c(15,16),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))

 
##### errorbars#####
mean440<-ddply(data,c("Day","DOM"), summarise,
           Mean_Chl=mean(DecAbsCoeff440, na.rm=TRUE),
           sd = sd(DecAbsCoeff440, na.rm=TRUE),
           n = sum(!is.na(DecAbsCoeff440)),
           se = sd/sqrt(n))

ggplot(mean440,aes(Day, Mean_Chl,shape=DOM,colour = DOM))+
  geom_point(size=2,position=pd)+
  geom_errorbar(aes(ymin=Mean_Chl-sd,ymax=Mean_Chl+sd), width=0.6, size=0.5, colour="black",position=pd)+
  scale_shape_manual(values= c(15,16),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))


#####smooth data#####
##lines & dots have the same colour
ggplot(data,aes(Day, DecAbsCoeff440,shape=DOM,colour = DOM))+
  geom_point(size=2)+
  geom_smooth (data=data,method="loess",span = 0.2,size = 0.5, alpha = 0.01,se=FALSE)+ 
  scale_shape_manual(values= c(15,16),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))

##lines are black
ggplot(data,aes(Day, DecAbsCoeff440,shape=DOM,colour = DOM))+
  geom_point(size=2)+
  geom_smooth (data=data,method="loess",span = 0.2,colour="black",size = 0.5, alpha = 0.01,se=FALSE)+
  scale_shape_manual(values= c(15,16),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))

##lines have different types according to the treatment
ggplot(data,aes(Day, DecAbsCoeff440,shape=DOM,colour = DOM,linetype=DOM))+
  geom_point(size=2)+
  geom_smooth (data=data,method="loess",span = 0.2,size = 0.5, alpha = 0.01,se=FALSE)+ 
  scale_shape_manual(values= c(15,16),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  scale_linetype_manual(values=c("solid","dotted"),labels=c("-DOM", "+DOM"))
###se=FALSE: turn the confidence interval off; 
###span= controls the wiggliness of the line (0=exceeding wiggly 1=not so wiggly)
###method='loess', does not work for datasets n>1000; other methods: e.g. lm


#####title#####
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  ggtitle('Absorption at 440 nm')


#####axes#####
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep=""))) # [] subscript
  #ylab(expression(paste("Chl ",italic("a "),  "(",mu,g, " ",L^-1,")", sep=""))) # a is written in italic

##no axis titel
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("  ")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep=""))) 

##y-axes is limited & labels are scientific
  ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
    geom_point(size=2)+
    scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
    xlab(expression(paste("Day")))+
    ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
    scale_y_continuous(limits=c(0,2),labels=scientific)
  
##y-axes is limited and the ticks are manually set
  ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
    geom_point(size=2)+
    scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
    xlab(expression(paste("Day")))+
    ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,2.1),breaks=round(seq(min(0), max(2.1), by = 0.3),1)) 
 
##x-axes is reversed shown
  ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
    geom_point(size=2)+
    scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
    xlab(expression(paste("Day")))+
    ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
    scale_x_reverse(limits=c(90, 50))
 

#####horizontal lines & vertical lines#####
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  geom_vline(xintercept=70,size=0.5, colour="black")+
  geom_hline(yintercept=1,size=0.5, colour="black",linetype="dotted")



#####themes & change titel & change legend#####
library(ggthemes)

##different themse
  ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
    geom_point(size=2)+
    scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
    xlab(expression(paste("Day")))+
    ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
    ggtitle('Absorption at 440 nm')+
    theme_bw()
  
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_excel() #a theme replicating the classic ugly gray charts in Excel
  
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_few()# theme from Stephen Few's "Practical Rules for Using Color in Charts"

ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_tufte()# no frame
  
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_solarized_2(light = FALSE) # dark theme

ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_classic()


##adapt themes
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_classic()+
    theme(
  axis.text.x=element_text(size=20,colour="black"),
  axis.text.y=element_text(size=20,colour="black"),
  axis.title.x=element_text(size=23,vjust=-0.5),
  axis.title.y=element_text(size=23,vjust=0.5),
  axis.ticks = element_line(colour="black",size=1),
  axis.ticks.length = unit(.25, "cm"))

##change titel appearance
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_classic()+
  theme(
  plot.title=element_text(hjust = 0.5,vjust=-2,size=20,colour='darkgreen',face='bold.italic',family='Times'))

##legend
ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_classic()+
  theme(legend.position = "none")# also possible: top,bottom,left,right

ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme_classic()+
  theme(legend.position=c(0,1),legend.justification = c(-0.1,1.1))+ #legend.justification: plot into the graphic area; Top right = c(1, 1), bottom left = c(0, 0)
  theme(legend.background=element_rect(fill="white", colour="black"))+ # border around the legend
  theme(legend.key=element_blank()) # Remove border around each item


#####transparent background#####

png(width=5000, height=2500, res=600,bg = "transparent", file="C:/Users/planktologie/Desktop/Figure 4.tiff")

Abs440<-ggplot(data,aes(Day, DecAbsCoeff440,colour = DOM))+
  geom_point(size=2)+
  scale_colour_manual(values= c('red','blue'),labels = c("- DOM", "+ DOM"))+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  ggtitle('Absorption at 440 nm')+
  theme(
    axis.line.x = element_line(color = "black",size=0.5), axis.line.y = element_line(color = "black",size=0.5),
    panel.background=element_rect(fill="transparent"),
    panel.grid.major=element_line(colour="transparent"),
    panel.grid.minor=element_line(colour="transparent"),
    plot.background = element_rect(fill = "transparent"),
    legend.background=element_rect(fill="transparent"),
    legend.key=element_blank())


plot(Abs440)
dev.off()


#####splitting data into subplots#####
##facet_wrap
ggplot(data,aes(Day, DecAbsCoeff440,shape = Daphnia,colour=DOM))+
  geom_point(size=2)+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,15))+
  scale_shape_manual(values= c(15,16),labels = c("- Daphnia", "+ Daphnia"))+
  scale_colour_manual(values=c("black","red"),labels = c("-DOM", "+DOM"))+
  facet_wrap( ~ Daphnia*DOM, ncol=2,nrow=2)

ggplot(data,aes(Day, DecAbsCoeff440,shape = Daphnia,colour=DOM))+
  geom_point(size=2)+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,15))+
  scale_shape_manual(values= c(15,16),labels = c("- Daphnia", "+ Daphnia"))+
  scale_colour_manual(values=c("black","red"),labels = c("-DOM", "+DOM"))+
  facet_wrap( ~ Daphnia*DOM, nrow=2,scale='free_y') # y scales vary across panels; also possible with free and free_x

##facet_grid
ggplot(data,aes(Day, DecAbsCoeff440,shape = Daphnia,colour=DOM))+
  geom_point(size=2)+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,15))+
  scale_shape_manual(values= c(15,16),labels = c("- Daphnia", "+ Daphnia"))+
  scale_colour_manual(values=c("black","red"),labels = c("-DOM", "+DOM"))+
  facet_grid(. ~ DOM*Daphnia,labeller=label_both)


#####arrange plots#####
library(cowplot)

png(width=5000, height=2500, res=600,bg = "transparent", file="C:/Users/planktologie/Desktop/Figure 4.tiff")

Abs_440<-ggplot(data,aes(Day, DecAbsCoeff440,shape = DOM,colour=DOM,linetype=DOM))+
  geom_point(size=2)+
  geom_smooth (data=data,span = 0.2, colour="black",size = 0.5, alpha = 0.01,se=FALSE)+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_440 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,15))+
  scale_shape_manual(values= c(1,2),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values=c("black","red"),labels = c("-DOM", "+DOM"))+
  scale_linetype_manual(values=c("solid","dotted"),labels=c("-DOM", "+DOM"))+
  
  theme(
    panel.background=element_rect(fill="transparent"),
    panel.border = element_blank(),axis.line = element_line(colour="black", size =0.5),
    panel.grid.major=element_line(colour="transparent"),
    panel.grid.minor=element_line(colour="transparent"),
    plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line.x = element_line(color = "black",size=0.5), axis.line.y = element_line(color = "black",size=0.5))+
  theme(axis.ticks = element_line(colour="black",size=0.5))+
  theme(axis.text.x=element_text(size=10,colour="black"))+
  theme(axis.text.y=element_text(size=10,colour="black"))+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.ticks = element_line(size = 0.5))+
  theme(legend.position = "none")


Abs_254<-ggplot(data,aes(Day, DecAbsCoeff254,shape = DOM,colour=DOM,linetype=DOM))+
  geom_point(size=2)+
  geom_smooth (data=data,span = 0.2, colour="black",size = 0.5, alpha = 0.01,se=FALSE)+
  xlab(expression(paste("Day")))+
  ylab(expression(paste("A"["_254 "] ,  "(", m^-1,")", sep="")))+
  scale_y_continuous(limits=c(0,15))+
  scale_shape_manual(values= c(1,2),labels = c("- DOM", "+ DOM"))+
  scale_colour_manual(values=c("black","red"),labels = c("-DOM", "+DOM"))+
  scale_linetype_manual(values=c("solid","dotted"),labels=c("-DOM", "+DOM"))+
  
  theme(
    panel.background=element_rect(fill="transparent"),
    panel.border = element_blank(),axis.line = element_line(colour="black", size =0.5),
    panel.grid.major=element_line(colour="transparent"),
    panel.grid.minor=element_line(colour="transparent"),
    plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line.x = element_line(color = "black",size=0.5), axis.line.y = element_line(color = "black",size=0.5))+
  theme(axis.ticks = element_line(colour="black",size=0.5))+
  theme(axis.text.x=element_text(size=10,colour="black"))+
  theme(axis.text.y=element_text(size=10,colour="black"))+
  theme(axis.title.x=element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.ticks = element_line(size = 0.5))+
  theme(legend.position = "none")

plot_grid(Abs_254,Abs_440, labels="auto",vjust=1,label_size=20, ncol = 2, align = 'h') # labels "auto": the labels are generated automaticaly wit a,b,c,.... When labels="AUTO" the automatically created letters are capitalized

dev.off()
