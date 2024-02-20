library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(dplyr)
library(ggpubr)
library(rstatix)
library(readxl)
setwd("/Users/rthrs/OneDrive - University of Cincinnati/PhD Data/Aim2/Opthalmoscope imaging/analysis R")
options(max.print=999999)
#Statistics
#all eye best resciprocal object distance
eye <- read.csv("blindanalysisruby_viablevalues_100p.csv")
eye


#histogram
#original data
ggplot(eye, aes(x = Best)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 0.07) +
  facet_grid(Groups ~ eyety)

#Densityplot
#original data
ggplot(data=eye, aes(x=Best, group=eyety, fill=eyety)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(Groups ~ eyety)

#QQplot
#original data
ggqqplot(eye, x = "Best",
         color = "eyety",ggtheme = theme_pubclean())+facet_grid(Groups ~ eyety)




#Violin, box and scatter plots for upper, best and lower rail positions for all four eyes

#defining the graph variables
ey <- ggplot(eye, aes(x = eyeno, y = Best))

#violin and box plot
finalplot <- ey + geom_violin(
  aes(fill= Groups), trim = FALSE,
  position = position_dodge(0.9) 
) +
  geom_boxplot(
    aes(color = NULL), width = 0.15,
    position = position_dodge(0.9))+
  geom_dotplot(
      aes(fill= NULL), trim = FALSE,
      binaxis='y', stackdir='center', dotsize = 0.5,
      position = position_dodge(0.05))+scale_x_discrete(limits=c("CLE1","TLE1","CLE2","TLE2","CRE1","TRE1","CRE2","TRE2"))

#removing background panels
finalplot+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

#scatterplot and boxplot
ey2<- ggplot(eye,aes(x=Groups, y= Best))
box <- ey2 + geom_boxplot(
  aes(color = NULL), width = 0.5, size = 0.5,
  position = position_dodge(0.8)
)+geom_dotplot(
    aes(fill = Groups), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.6,
    position = position_dodge(0.8),binwidth = 0.023)
#removing background panels
box+ facet_grid(.~eyety)+ theme(text = element_text(size = 25),axis.text.x=element_blank())+ ylab ("Object distance (cm)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)
  
  
#Shapirowilks test
with(eye, shapiro.test(Best[eyeno == "CLE1"]))
with(eye, shapiro.test(Best[eyeno == "TLE1"]))
with(eye, shapiro.test(Best[eyeno == "CLE2"]))
with(eye, shapiro.test(Best[eyeno == "TLE2"]))
with(eye, shapiro.test(Best[eyeno == "CRE1"]))
with(eye, shapiro.test(Best[eyeno == "TRE1"]))
with(eye, shapiro.test(Best[eyeno == "CRE2"]))
with(eye, shapiro.test(Best[eyeno == "TRE2"]))


##pairwise wilcoxon for the data grouped by eye id
compare_means(Best~ eyeno, data = eye, 
              group.by = "eyety")

#kruskalwallis
k<- kruskal.test(Best ~ eyeno, data = eye)
k


##pairwise t test for the data grouped by eye id
compare_means(Best~ eyeno, data = eye, 
              group.by = "eyety", method= "t.test")

#split by sample id for controls
bc<- read.csv("blindcontrol.csv")
bc
facet <- ggplot(bc, aes(x = eyeid, y = Best))
fac <- facet + geom_dotplot(
  aes(fill = eyeid), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.4,
  position = position_dodge(0.8), binwidth = 0.023)
f <- fac + facet_grid( . ~ ID)
f

#split by sample ID for tests
bt<- read.csv("blindtest.csv")
bt
factest <- ggplot(bt, aes(x = eyeid, y = Best))
fa <- factest + geom_dotplot(
  aes(fill = eyeid), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.4,
  position = position_dodge(0.8), binwidth = 0.023)
test <- fa + facet_grid( . ~ ID)
test

#sampleviability

library(ggplot2)
library(viridis)
library(hrbrthemes)

vie1<- read.csv("viability.csv")
vie1

vip <- ggplot(data = vie1, aes(x = Group, y = Viability)) +
  geom_col(aes(fill = Image), width = 0.8)+facet_wrap(~Eyeno)+
  scale_fill_manual(values=c("red","blue"))
vip + theme(text = element_text(size = 25)) + ylab ("% Sample viability")+ xlab ("Groups")

vie2<- read.csv("viabilitye2.csv")
vie2

vi <- ggplot(data = vie2, aes(x = Group, y = Viability)) +
  geom_col(aes(fill = Image), width = 0.8)+facet_wrap(~Eyeno)+
  scale_fill_manual(values=c("red","blue"))
vi + theme(text = element_text(size = 25)) + ylab ("% Sample viability")+ xlab ("Groups")



#####blind with mm correction with blind analyzed hunting behavior

#Statistics
#all eye best resciprocal object distance
ey <- read.csv("blindanalysisrubyviablevalueshyp.csv")
ey


#histogram
#original data
ggplot(ey, aes(x = Best)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 0.07) +
  facet_grid(Groups ~ eyety)

#Densityplot
#original data
ggplot(data=ey, aes(x=Best, group=eyety, fill=eyety)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(Groups ~ eyety)

#QQplot
#original data
ggqqplot(ey, x = "Best",
         color = "eyety",ggtheme = theme_pubclean())+facet_grid(Groups ~ eyety)




#box and scatter plots for upper, best and lower rail positions for all four eyes

#defining the graph variables
eyr <- ggplot(ey, aes(x = eyeno, y = Best))

#scatterplot and boxplot
ey2<- ggplot(ey,aes(x=Groups, y= Best))
box <- ey2 + geom_boxplot(
  aes(fill = Groups), width = 0.9, size = 0.9,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.6,
  position = position_dodge(0.8),binwidth = 0.023)+scale_fill_manual(values=c("red", "blue"))
#removing background panels
box+ facet_grid(.~eyety)+ theme(text = element_text(size = 25),axis.text.x=element_blank())+ ylab ("Object distance (mm)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)


#Shapirowilks test
with(ey, shapiro.test(Best[eyeno == "CLE1"]))
with(ey, shapiro.test(Best[eyeno == "TLE1"]))
with(ey, shapiro.test(Best[eyeno == "CLE2"]))
with(ey, shapiro.test(Best[eyeno == "TLE2"]))
with(ey, shapiro.test(Best[eyeno == "CRE1"]))
with(ey, shapiro.test(Best[eyeno == "TRE1"]))
with(ey, shapiro.test(Best[eyeno == "CRE2"]))
with(ey, shapiro.test(Best[eyeno == "TRE2"]))


##pairwise wilcoxon for the data grouped by eye id
compare_means(Best~ eyeno, data = ey, 
              group.by = "eyety")

#kruskalwallis
k<- kruskal.test(Best ~ eyeno, data = ey)
k


##pairwise t test for the data grouped by eye id
compare_means(Best~ eyeno, data = eye, 
              group.by = "eyety", method= "t.test")


#
#
#
#defining the graph variables
eyr <- ggplot(ey, aes(x = eyeno, y = Best))

#scatterplot and boxplot
ey2<- ggplot(eyr,aes(x=Groups, y= Best))
box <- ey2 + geom_boxplot(
  aes(fill = Groups), width = 0.9, size = 0.8,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.4,
  position = position_dodge(0.8),binwidth = 0.023)+scale_fill_manual(values=c("red", "blue"))
#removing background panels
box+ facet_grid(.~eyety)+ theme(text = element_text(size = 20),axis.text.x=element_blank())+ ylab ("Object distance (mm)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)





##
#
#
#Hanging drops
#Statistics
#all eye best resciprocal object distance
hg <- read.csv("osmosis_edge_sharpness_ATM.csv")
hg


#histogram
#original data
ggplot(hg, aes(x = bfc)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Groups ~ eyety)

#Densityplot
#original data
ggplot(data=hg, aes(x=bfc, group=eyety, fill=eyety)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(Groups ~ eyety)

#QQplot
#original data
ggqqplot(hg, x = "bfc",
         color = "eyety",ggtheme = theme_pubclean())+facet_grid(Groups ~ eyety)




#box and scatter plots for back surface focal distance for all four eyes

#scatterplot and boxplot
hgd<- ggplot(hg,aes(x=Groups, y= bfc))
boxhg <- hgd + geom_boxplot(
  aes(fill = Groups), width = 0.9, size = 0.9,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = bfc), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.6,
  position = position_dodge(0.8),binwidth = 0.023)+scale_fill_manual(values=c("red", "blue"))
#removing background panels
boxhg+ facet_grid(.~eyety)+ theme(text = element_text(size = 25),axis.text.x=element_blank())+ ylab ("back focal distance(um)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)


#Shapirowilks test
with(hg, shapiro.test(bfc[eyeno == "CLE1"]))
with(hg, shapiro.test(bfc[eyeno == "TLE1"]))
with(hg, shapiro.test(bfc[eyeno == "CLE2"]))
with(hg, shapiro.test(bfc[eyeno == "TLE2"]))
with(hg, shapiro.test(bfc[eyeno == "CRE1"]))
with(hg, shapiro.test(bfc[eyeno == "TRE1"]))
with(hg, shapiro.test(bfc[eyeno == "CRE2"]))
with(hg, shapiro.test(bfc[eyeno == "TRE2"]))


##pairwise wilcoxon for the data grouped by eye id
compare_means(bfc~ eyeno, data = hg, 
              group.by = "eyety")

#kruskalwallis
k<- kruskal.test(Best ~ eyeno, data = ey)
k


##pairwise t test for the data grouped by eye id
compare_means(bfc~ eyeno, data = hg, 
              group.by = "eyety", method= "t.test")


#
#
#
#defining the graph variables
hgb<- ggplot(hg, aes(x = eyeno, y = bfc))

#scatterplot and boxplot
hgbd<- ggplot(hg,aes(x=Groups, y= bfc))
boxh <- hgbd + geom_boxplot(
  aes(fill = Groups), width = 0.9, size = 0.8,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 2.0,
  position = position_dodge(0.8),binwidth = 3)+scale_fill_manual(values=c("red", "blue"))
#removing background panels
boxh+ facet_grid(.~eyety)+ theme(text = element_text(size = 20),axis.text.x=element_blank())+ ylab ("Back surface focal distance (um)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)

