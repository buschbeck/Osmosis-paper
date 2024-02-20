library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(ggpubr)
library(rstatix)
library(dplyr)
library(readxl)
setwd("/Users/rthrs/OneDrive - University of Cincinnati/PhD Data/Aim2/huntingexp")
hp <- read.csv("strikedis.csv")
hp

#Statistics

#histogram
#original data
ggplot(hp, aes(x = Average)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 1) +
  facet_grid(.~ Group)

#Densityplot
#original data
ggplot(data=hp, aes(x=Average, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(.~arena)

#QQplot
#original data
ggqqplot(hp, x = "Average",
         color = "arena",ggtheme = theme_pubclean())+facet_grid(.~Group)




#defining the graph variables
h <- ggplot(hp, aes(x = Group, y = Average))

sd <- h + geom_boxplot(
  aes(fill= Group), width = 0.5, size = 0.5,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = name), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.7,
  position = position_dodge(0.43),binwidth=0.6)
#removing background panels
sd+facet_grid ( . ~ arena)+theme(text = element_text(size = 25),legend.position= "blank")+  ylab ("Strike distance (mm)") + xlab ("Groups")+stat_compare_means(label =  "p.format",size = 7, label.x = 1.4) 


#Shapirowilks test
with(hp, shapiro.test(Average[facetvar == "HC"]))
with(hp, shapiro.test(Average[facetvar == "VC"]))
with(hp, shapiro.test(Average[facetvar == "HT"]))
with(hp, shapiro.test(Average[facetvar == "VT"]))

compare_means(Average~facetvar, hp, paired = FALSE,
              group.by = NULL, ref.group = NULL)

#filtered by arena
#Horizontal
strdhz <- hp %>%
  filter(arena == "Horizontal")
strdhz
#defining the graph variables
hz <- ggplot(strdhz, aes(x = Group, y = Average))

sdhz <- hz + geom_boxplot(
  aes(fill= Group), width = 0.5, size = 0.5,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.6,
  position = position_dodge(0.43),binwidth=0.6)+scale_fill_manual(values=c("red","blue"))
#removing background panels
sdhz+theme(axis.line = element_line(colour = "black"))+ylim(2,8) + ylab ("Strike distance (mm)") + xlab ("Groups")+theme(text = element_text(size = 20))


#Vertical
strdvt <- hp %>%
  filter(arena == "Vertical")
strdvt
#defining the graph variables
vt <- ggplot(strdvt, aes(x = Group, y = Average))

sdvt <- vt + geom_boxplot(
  aes(fill= Group), width = 0.5, size = 0.5,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.6,
  position = position_dodge(0.43),binwidth=0.6)+scale_fill_manual(values=c("red","blue"))
#removing background panels
sdvt+theme(axis.line = element_line(colour = "black"))+  ylab ("Strike distance (mm)") + xlab ("Groups")+theme(text = element_text(size = 20))




#plotting strike 1
sone <- read.csv("strike1.csv")
sone

#defining the graph variables
so <- ggplot(sone, aes(x = Group, y = Average))

s <- so+ geom_boxplot(
  aes(color = Group), width = 0.5, size = 0.4,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.5,
  position = position_dodge(0.8))
#removing background panels
s+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#####
#plotting Auggie's blind analysis data
ag <- read.csv("augsd.csv")
ag

#Statistics

#histogram
#original data
ggplot(ag, aes(x = Averagesd)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 1) +
  facet_grid(.~ Group)

#Densityplot
#original data
ggplot(data=ag, aes(x=Averagesd, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(.~arena)

#QQplot
#original data
ggqqplot(ag, x = "Averagesd",
         color = "arena",ggtheme = theme_pubclean())+facet_grid(.~Group)




#defining the graph variables
agh <- ggplot(ag, aes(x = Group, y = Averagesd))

agsd <- agh + geom_boxplot(
  aes(fill= Group), width = 0.5, size = 0.5,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = name), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.7,
  position = position_dodge(0.43),binwidth=0.6)
#removing background panels
agsd+facet_grid ( . ~ arena)+theme(text = element_text(size = 25),legend.position= "blank")+  ylab ("Strike distance (mm)") + xlab ("Groups")+stat_compare_means(label =  "p.format",size = 7, label.x = 1.4)+
  scale_y_continuous(breaks=seq(0,8,by=2.0),limits = c(0,8))


#Shapirowilks test
with(ag, shapiro.test(Averagesd[facetvar == "HC"]))
with(ag, shapiro.test(Averagesd[facetvar == "VC"]))
with(ag, shapiro.test(Averagesd[facetvar == "HT"]))
with(ag, shapiro.test(Averagesd[facetvar == "VT"]))

compare_means(Averagesd~facetvar, ag, paired = FALSE,
              group.by = NULL, ref.group = NULL)

#filtered by arena
#HZ
sdaghz <- ag %>%
  filter(arena == "Horizontal")
sdaghz
#defining the graph variables
aghz <- ggplot(sdaghz, aes(x = Group, y = Averagesd))

shz <- aghz + geom_boxplot(
  aes(fill= Group), width = 0.7, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.3,
  position = position_dodge(0.43),binwidth=0.6)+scale_fill_manual(values=c("red","blue"))
#removing background panels
shz+theme(axis.line = element_line(colour = "black"))+  ylab ("Strike distance (mm)") +scale_y_continuous(breaks=seq(0,8,by=2.0),limits = c(1,7))+ xlab ("Groups")+theme(text = element_text(size = 20))


#VT
sdagvt <- ag %>%
  filter(arena == "Vertical")
sdagvt
#defining the graph variables
agvt <- ggplot(sdagvt, aes(x = Group, y = Averagesd))

svt <- agvt + geom_boxplot(
  aes(fill= Group), width = 0.7, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(color = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.5,
  position = position_dodge(0.43),binwidth=0.6)+scale_fill_manual(values=c("red","blue"))
#removing background panels
svt+theme(axis.line = element_line(colour = "black"))+  ylab ("Strike distance (mm)") +scale_y_continuous(breaks=seq(0,10,by=2.0),limits = c(1,10.6))+ xlab ("Groups")+theme(text = element_text(size = 20))



