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
he <- read.csv("Huntingsuccess.csv")
he

#Statistics

#histogram
#original data
ggplot(he, aes(x = Hunting.success)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 29) +
  facet_grid(Group ~ Arena )

#Densityplot
#original data
ggplot(data=he, aes(x=Hunting.success, group=Group, fill=Group)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(Group ~ Arena)

#QQplot
#original data
ggqqplot(he, x = "Hunting.success",
         color = "Group",ggtheme = theme_pubclean())+facet_grid(Group ~ Arena)




#defining the graph variables
hunt <- ggplot(he, aes(x = Group, y = Hunting.success))

vihunt <- hunt + geom_violin(
  aes(color = Group), trim = FALSE,
  position = position_dodge(0.9) 
) +
  geom_boxplot(
    aes(color = Group), width = 0.15,
    position = position_dodge(0.9))
vihunt + facet_grid ( . ~ Arena)

#box plot with dots colored with IDs
bx <- hunt + geom_boxplot(
  aes(color = NULL), width = 0.4, size = 0.6,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill= ID), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.5), binwidth = 5)

fsbx <- bx + facet_grid( . ~ Arena)+theme(text = element_text(size = 25),legend.position= "blank")+  ylab ("% Hunting success") + xlab ("Groups")+stat_compare_means(method= "t.test",label =  "p.format",size = 7, label.x = 1.5) 
fsbx 

#Shapirowilks test
with(he, shapiro.test(Hunting.success[normality == "CHorizontal"]))
with(he, shapiro.test(Hunting.success[normality == "CVertical"]))
with(he, shapiro.test(Hunting.success[normality == "THorizontal"]))
with(he, shapiro.test(Hunting.success[normality == "TVertical"]))

#means
d1s<-group_by(he, normality) %>%
  summarise(
    count= n(),
    mean= mean(Hunting.success, na.rm= TRUE),
    sd= sd(Hunting.success, na.rm= TRUE)
  )
d1s


##t.test for all
compare_means(Hunting.success~normality, he, method = "wilcox.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)
my_comparisonshe <- list( c("CHorizontal", "THorizontal"), c("Cvertical", "Tvertical"))


phe <- ggboxplot(he, x = "normality", y = "Hunting.success",
               color = normality, palette = "jco",size= 3.5)+geom_jitter(data=he, mapping=aes(x=normality, y=Hunting.success), size=3.5, position=position_jitter(width=0.2, height=0)) +stat_compare_means(method = "t.test",comparisons = my_comparisonshe, label.y = c(110, 110))+stat_summary(fun= mean, geom="point", shape=13, size=8, color="black", fill="white")+ xlab ("Groups")+ ylab("% Hunting success") + theme(text = element_text(size = 25))+facet_grid( . ~ Arena)
phe

#
#Latency data
lat <- read.csv("latency.csv")
lat

#defining the graph variables
la <- ggplot(lat, aes(x = group, y = latencymin))

vila <- la + geom_violin(
  aes(color = group), trim = FALSE,
  position = position_dodge(0.9) 
) +
  geom_boxplot(
    aes(color = group), width = 0.15,
    position = position_dodge(0.9))
vila + facet_grid ( . ~ arena)

ly <- la + geom_boxplot(
  aes(color = NULL), width = 0.4, size = 0.6,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = id), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.285), binwidth = 1.5)
#removing background panels
ly+ facet_grid ( . ~ arena, scales = 'free')+theme(text = element_text(size = 25),legend.position= "blank")+  ylab ("Latency (minutes)") + xlab ("Groups")+stat_compare_means(label =  "p.format",size = 7, label.x = 1.3) 



#Shapirowilks test
with(lat, shapiro.test(latencymin[normality == "Chorizontal"]))
with(lat, shapiro.test(latencymin[normality == "Cvertical"]))
with(lat, shapiro.test(latencymin[normality == "Thorizontal"]))
with(lat, shapiro.test(latencymin[normality == "Tvertical"]))

compare_means(latencymin~normality, lat, method = "wilcox.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #usedforsicb
compare_means(latencymin~normality, lat, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

my_comparisonsly <- list( c("Chorizontal", "Thorizontal"), c("Cvertical", "Tvertical"))

ply <- ggboxplot(lat, x = "group", y = "latencymin",
                 color = "normality", palette = "jco",size= 3.5)+geom_jitter(data=he, mapping=aes(x=normality, y=latencymin), size=3.5, position=position_jitter(width=0.2, height=0))+stat_compare_means(method = "wilcox.test",comparisons = my_comparisonsly, label.y = c(30, 30))+ xlab ("Groups")+ ylab("Latency (min)") + theme(text = element_text(size = 25))+facet_grid( . ~ arena)
ply



#Plotting larval activity of all larvae
pa <- read.csv("path.csv")
pa

#defining the graph variables
pat <- ggplot(pa, aes(x = group, y = path.length))

p <- pat+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
p+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("Distance travelled (cm)")+theme(text = element_text(size = 20))
  


#Statistics

#histogram
#original data
ggplot(pa, aes(x = path.length)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 50)

#Densityplot
#original data
ggplot(data=pa, aes(x=path.length, group=group, fill=group)) +
  geom_density(adjust=1.5, alpha=.4)

#QQplot
#original data
ggqqplot(pa, x = "path.length",
         color = "group",ggtheme = theme_pubclean())

#Testing normal distribution
with(pa, shapiro.test(path.length[group == "Control"]))
with(pa, shapiro.test(path.length[group == "InsectRingers"]))


compare_means(path.length..cm.~group, pa, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

#plotting the lame box plot
my_comparisonsp <- list( c("control", "hyperosmotic"))

path <- ggboxplot(pa, x = "group", y = "path.length",
                 fill = "group",size= 3.5)+geom_jitter(data=pa, mapping=aes(x=group, y=path.length, fill=NULL), size=3.5, position=position_jitter(width=0.2, height=0))+ xlab ("Groups")+ ylab("Distance travelled (cm)/ five minutes") + theme(text = element_text(size = 25))+scale_fill_manual(c("red","blue"))
path

#coloring dots like other plots

bxp <- pat + geom_boxplot(
  aes(color = NULL), width = 0.4, size = 0.6,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill= ID), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 3.5,
  position = position_dodge(0.5), binwidth = 5)

fsbxp <- bxp+theme(text = element_text(size = 25),legend.position= "blank")+  ylab ("Distance (cm)") + xlab ("Groups")+stat_compare_means(method= "t.test",label =  "p.format",size = 5, label.x = 1.5) 
fsbxp 


#Plotting larval activity of the larvae with good posture
pag <- read.csv("pathgoodpos.csv")
pag

#defining the graph variables
patg <- ggplot(pag, aes(x = group, y = path.length..cm.))

pg <- patg+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
pg+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("Distance travelled (cm)")+theme(text = element_text(size = 20))


#QQplot
#original data
ggqqplot(pag, x = "path.length..cm.",
         color = "group",ggtheme = theme_pubclean())

#Testing normal distribution
with(pag, shapiro.test(path.length..cm.[group == "Control"])) #Normal
with(pag, shapiro.test(path.length..cm.[group == "InsectRingers"])) #Normal

#Ftest
var.test(path.length..cm. ~ group, pag, 
         alternative = "two.sided") #Not variable

#t.test
compare_means(path.length..cm.~group, pag, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #USED FOR THE PAPER
#wilcoxon's test
compare_means(path.length..cm.~group, pag, method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL) #NOT USED

#
#
#Visual response
#Plotting visual response instances in vertical set up all points
lc <- read.csv("larvalcategory.csv")
lc

#defining the graph variables
lcat <- ggplot(lc, aes(x =larvalid , y = visualresponse))

lar <- lcat+ geom_boxplot(
  aes(fill = larvalid), width = 0.5, size = 0.7,
  position = position_dodge(0.8),
)+geom_dotplot(
  aes(fill = posture), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))
#removing background panels
lar+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_fill_manual(values = c("purple", "red","blue","black"))+
  ylab ("Number of visual responses")+stat_compare_means(method= "t.test",label =  "p.format",size = 5, label.x = 1.5) 

#
#
#Plotting visual response instances in vertical set up good pose
lcg <- read.csv("larvalcatgoodpos.csv")
lcg

#defining the graph variables
lcag <- ggplot(lcg, aes(x =larvalid , y = visualresponse))

larg <- lcag+ geom_boxplot(
  aes(fill = larvalid), width = 0.5, size = 0.7,
  position = position_dodge(0.8),
)+geom_dotplot(
  aes(fill = posture), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))
#removing background panels
larg+theme(text = element_text(size = 25))+scale_fill_manual(values = c("red","black","blue"))+
  ylab ("Number of visual responses")+stat_compare_means(method= "t.test",label =  "p.format",size = 5, label.x = 1.5) 

#QQplot
#original data
ggqqplot(lcg, x = "visualresponse",
         color = "larvalid",ggtheme = theme_pubclean())

#Testing normal distribution
with(lcg, shapiro.test(visualresponse[larvalid == "Control"])) #Normal
with(lcg, shapiro.test(visualresponse[larvalid == "InsectRinger"])) #Normal

#Ftest
var.test(visualresponse ~ larvalid, lcg, 
         alternative = "two.sided") #Not variable

#t.test
compare_means(visualresponse ~ larvalid, lcg, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #USED FOR THE PAPER
#wilcoxon's test
compare_means(visualresponse ~ larvalid, lcg, method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL) #NOT USED
#
#Block diagram of larval catagory
catg<- read.csv("categorization.csv")
catg

p <- ggplot(data=catg, aes(x=type, y=classif, fill=group)) +
  geom_bar(stat="identity", color="black", position=position_dodge())
p+scale_fill_manual(values=c('red','blue'))+theme(text = element_text(size = 25))+ ylab ("% classification")

#Block diagram of striking percent
#Verical
strp<- read.csv("strikecat.csv")
strp

strike <- ggplot(data = strp, aes(x = group, y = classif)) +
  geom_col(aes(fill = type), width = 0.8)
strike + theme(text = element_text(size = 25)) + ylab ("% Attempted strikes")+ xlab ("Groups")+
  scale_fill_manual(values = c("darkgray", "azure4"))

#Horizontal

sthz<- read.csv("strikecathz.csv")
sthz

strhz <- ggplot(data = sthz, aes(x = group, y = classif)) +
  geom_col(aes(fill = type), width = 0.8)
strhz + theme(text = element_text(size = 25)) + ylab ("% Attempted strikes")+ xlab ("Groups")+
  scale_fill_manual(values = c("darkgray", "azure4"))


#
#
#
#horizontal missed strike
blinhz <- read.csv("AuggieBlindanalysisbehavior2022.csv")
blinhz

#defining the graph variables
msbh <- ggplot(blinhz, aes(x = group, y = missed.strikes))

msh <- msbh+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
msh+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("No. of strikes")+theme(text = element_text(size = 20))+ scale_y_continuous(breaks=seq(0,9.5,by=1.0),limits = c(0,10.0))

#t.test
compare_means(missed.strikes~group, blinhz, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

#
#Horizontal total strikes
#defining the graph variables
tsbh <- ggplot(blinhz, aes(x = group, y = total.strikes))

tsh <- tsbh+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
tsh+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("No. of strikes")+theme(text = element_text(size = 20))+ scale_y_continuous(breaks=seq(0,10,by=2.0),limits = c(0,11.0))

#QQplot
#original data
ggqqplot(blinhz, x = "total.strikes",
         color = "group",ggtheme = theme_pubclean())

#Testing normal distribution
with(blinhz, shapiro.test(total.strikes[group == "control"])) #Normal
with(blinhz, shapiro.test(total.strikes[group == "test"])) #Normal

#Ftest
var.test(total.strikes ~ group, blinhz, 
         alternative = "two.sided") #Not variable

#t.test
compare_means(total.strikes ~ group, blinhz, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #USED FOR THE PAPER
#wilcoxon's test
compare_means(total.strikes ~ group, blinhz, method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL) #NOT USED
#

#
#
#
#vertical missed strike
blinvrt <- read.csv("augblindvertical.csv")
blinvrt

#defining the graph variables
msb <- ggplot(blinvrt, aes(x = group, y = missed.strikes))

ms <- msb+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
ms+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("No. of strikes")+theme(text = element_text(size = 20))

#t.test
compare_means(missed.strikes~group, blinvrt, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

t.test(total.strikes~group, blinvrt, var.equal = TRUE)
#
#
#Vertical total strikes 
tsb <- ggplot(blinvrt, aes(x = group, y = total.strikes))

ts <- tsb+ geom_boxplot(
  aes(fill = group), width = 0.5, size = 0.7,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.8,
  position = position_dodge(0.8))+scale_fill_manual(values=c("red2", "blue"))
#removing background panels
ts+ stat_compare_means(label =  "p.format",size=7, label.x = 1.25)+
  ylab("No. of strikes")+theme(text = element_text(size = 20))+scale_y_continuous(breaks=seq(0,10,by=2.0),limits = c(0,11))

#QQplot
#original data
ggqqplot(blinvrt, x = "total.strikes",
         color = "group",ggtheme = theme_pubclean())

#Testing normal distribution
with(blinvrt, shapiro.test(total.strikes[group == "control"])) #Normal
with(blinvrt, shapiro.test(total.strikes[group == "test"])) #Normal

#Ftest
var.test(total.strikes ~ group, blinvrt, 
         alternative = "two.sided") #Not variable

#t.test
compare_means(total.strikes ~ group, blinvrt, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #USED FOR THE PAPER
#wilcoxon's test
compare_means(total.strikes ~ group, blinvrt, method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL) #NOT USED


#
#
#opthalmoscope blind analysis

#####blind with mm correction

#Statistics
#all eye best resciprocal object distance
bey <- read.csv("blindanalysisbehaviorhuntingsuccess.csv")
bey


#histogram
#original data
ggplot(bey, aes(x = Best)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 0.07) +
  facet_grid(Groups ~ eyety)

#Densityplot
#original data
ggplot(data=bey, aes(x=Best, group=eyety, fill=eyety)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(Groups ~ eyety)

#QQplot
#original data
ggqqplot(bey, x = "Best",
         color = "eyety",ggtheme = theme_pubclean())+facet_grid(Groups ~ eyety)




#box and scatter plots for upper, best and lower rail positions for all four eyes

#defining the graph variables
beyr <- ggplot(bey, aes(x = eyeno, y = Best))

#scatterplot and boxplot
bey2<- ggplot(bey,aes(x=Groups, y= Best))
boxbl <- bey2 + geom_boxplot(
  aes(fill = Groups), width = 0.9, size = 0.9,
  position = position_dodge(0.9)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.6,
  position = position_dodge(0.8),binwidth = 0.023)+scale_fill_manual(values=c("red", "blue"))
#removing background panels
boxbl+ facet_grid(.~eyety)+ theme(text = element_text(size = 19),axis.text.x=element_blank())+ ylab ("Object distance (mm)")+ xlab ("Groups")+stat_compare_means(label =  "p.format",size=7, label.x = 1.25)


#Shapirowilks test
with(bey, shapiro.test(Best[eyeno == "CLE1"]))
with(bey, shapiro.test(Best[eyeno == "TLE1"]))
with(bey, shapiro.test(Best[eyeno == "CLE2"]))
with(bey, shapiro.test(Best[eyeno == "TLE2"]))
with(bey, shapiro.test(Best[eyeno == "CRE1"]))
with(bey, shapiro.test(Best[eyeno == "TRE1"]))
with(bey, shapiro.test(Best[eyeno == "CRE2"]))
with(bey, shapiro.test(Best[eyeno == "TRE2"]))


##pairwise wilcoxon for the data grouped by eye id
compare_means(Best~ eyeno, data = bey, 
              group.by = "eyety")

#kruskalwallis
k<- kruskal.test(Best ~ eyeno, data = bey)
k


##pairwise t test for the data grouped by eye id
compare_means(Best~ eyeno, data = bey, 
              group.by = "eyety", method= "t.test")


