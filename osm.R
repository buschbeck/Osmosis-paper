library(ggplot2)
library(ggsignif)
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
library(FSA)
setwd("/Users/rthrs/OneDrive - University of Cincinnati/PhD Data/Aim2/Osmolarity measurements")
os <- read.csv("osm.csv")
os

#Osmolality values less than 50 have been removed
#defining the graph variables
o <- ggplot(os, aes(x = Stage, y = Osmolality))

#violin and box plot
osmoplot <- o + geom_violin(
  aes(fill= Stage), trim = FALSE,
  position = position_dodge(0.9) 
) +
  geom_boxplot(
    aes(color = NULL), width = 0.15,
    position = position_dodge(0.9))+scale_x_discrete(limits=c("Second day1", "Second day2", "Second day3", "Fresh molt third","Third day1","Third day2"))
osmoplot

#box and dot plot
x <- o + geom_boxplot(
  aes(color = Stage), width = 0.5, size = 0.4,
  position = position_dodge(0.8)
)+geom_dotplot(
  aes(fill = NULL), trim = FALSE,
  binaxis='y', stackdir='center', dotsize = 0.5,
  position = position_dodge(0.8),binwidth = 15)
#removing background panels
x+theme(text = element_text(size = 25),axis.text.x=element_blank())+ ylab ("Osmolality (mOsm)")+ xlab ("Instar Stage")+stat_compare_means(label =  "p.signif", label.x = 1.5)+scale_x_discrete(limits=c("Second day1", "Second day2", "Second day3", "Fresh molt third","Third day1","Third day2"))

#Statistics

#histogram
#original data
ggplot(os, aes(x = Osmolality)) +
  geom_histogram(fill = "white", colour = "black",binwidth= 50) +
  facet_grid(. ~ Stage)

#logtransformed
ggplot(os, aes(x = logosm)) +
  geom_histogram(fill = "blue", colour = "black",binwidth= 0.05) +
  facet_grid(. ~ Stage)

#Densityplot
#original data
ggplot(data=os, aes(x=Osmolality, group=Stage, fill=Stage)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(. ~ Stage)

#logtransformed
ggplot(data=os, aes(x=logosm, group=Stage, fill=Stage)) +
  geom_density(adjust=1.5, alpha=.4)+facet_grid(. ~ Stage)

#QQplot
#original data
ggqqplot(os, x = "Osmolality",
         color = "Stage", facet.by = "Stage",
         ggtheme = theme_pubclean())+facet_grid(. ~ Stage)


#logtransformed
ggqqplot(os, x = "logosm",
         color = "Stage", facet.by = "Stage",
         ggtheme = theme_pubclean())


#Shapirowilks test for normality for original data
with(os, shapiro.test(Osmolality[Stage == "Second day1"]))
with(os, shapiro.test(Osmolality[Stage == "Second day2"]))
with(os, shapiro.test(Osmolality[Stage == "Fresh molt third"]))
with(os, shapiro.test(Osmolality[Stage == "Third day1"]))
with(os, shapiro.test(Osmolality[Stage == "Second day3"]))
with(os, shapiro.test(Osmolality[Stage == "Third day2"]))

#Shapirowilks test for normality for log transformed
with(os, shapiro.test(logosm[Stage == "Second day1"]))
with(os, shapiro.test(logosm[Stage == "Second day2"]))
with(os, shapiro.test(logosm[Stage == "Fresh molt third"]))
with(os, shapiro.test(logosm[Stage == "Third day1"]))


#Summary stats of original data
d1s<-group_by(os, Stage) %>%
  summarise(
    count= n(),
    mean= mean(Osmolality, na.rm= TRUE),
    sd= sd(Osmolality, na.rm= TRUE),
    median = median(Osmolality, na.rm = TRUE),
    IQR = IQR(Osmolality, na.rm = TRUE)
  )
d1s

#Summary stats of log transformed data
logs<-group_by(os, Stage) %>%
  summarise(
    count= n(),
    mean= mean(logosm, na.rm= TRUE),
    sd= sd(logosm, na.rm= TRUE),
    median = median(logosm, na.rm = TRUE),
    IQR = IQR(logosm, na.rm = TRUE)
  )
logs

#Kruskal wallis test for original data
kruskal.test(Osmolality ~ Stage, data = os) #significant

#Kruskal wallis test for log transformed data
kruskal.test(logosm ~ Stage, data = os) #significant


##Wilcoxon test for original data
compare_means(Osmolality~Stage, os, method = "wilcox.test", exact= TRUE, paired = FALSE,
              group.by = NULL, ref.group = NULL)
my_comparisonsw <- list(c("Second day3","Third day1"), c("Fresh molt third", "Third day1"))

p <- ggboxplot(os, x = "Stage", y = "Osmolality",
                color = "Stage", palette = "jco",size= 2.0)+geom_jitter(data=os, mapping=aes(x=Stage, y=Osmolality), size=4.0, position=position_jitter(width=0.2, height=0)) +stat_compare_means(method= "wilcox.test",exact = TRUE,comparisons = my_comparisonsw,size=5.7, label.y = c(410, 380))+stat_summary(fun= mean, geom="point", shape=10, size=8, color="black", fill="white")+ xlab ("Instar stage")+ ylab("Osmolality (mOsm)") +theme_gray(base_size = 14)+ theme(text = element_text(size = 25),axis.text.x=element_blank())+scale_x_discrete(limits=c("Second day1", "Second day2", "Second day3", "Fresh molt third","Third day1","Third day2"))
p

#t test for original data
compare_means(Osmolality~Stage, os,method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

my_comparisonst <- list( c("Second day3", "Third day1"), c("Fresh molt third", "Third day1"))

p <- ggboxplot(os, x = "Stage", y = "Osmolality",
               color = "Stage", palette = "jco",size= 3.5)+geom_jitter(data=os, mapping=aes(x=Stage, y=Osmolality), size=3.5, position=position_jitter(width=0.2, height=0)) +stat_compare_means(method = "t.test",comparisons = my_comparisonst, label.y = c(400, 380))+stat_summary(fun= mean, geom="point", shape=13, size=8, color="black", fill="white")+ xlab ("Instar stage")+ ylab("Osmolality (mOsm)") + theme(text = element_text(size = 25))
p



#
#
#
#all points including verification data
vos <- read.csv("allosm.csv")
vos

#QQplot
#all data
ggqqplot(vos, x = "Osmolality",
         color = "Stage", facet.by = "Stage",
         ggtheme = theme_pubclean())+facet_grid(. ~ Stage)

#Test variance with F-test
varf <- vlogis %>%
  filter(srno == "f")
vlogif
var.test(Osmolality ~ Stage, vos, 
         alternative = "two.sided")
#Shapirowilks test for normality for original data
with(vos, shapiro.test(Osmolality[Stage == "Second day1"]))
with(vos, shapiro.test(Osmolality[Stage == "Second day2"]))
with(vos, shapiro.test(Osmolality[Stage == "Fresh molt"]))
with(vos, shapiro.test(Osmolality[Stage == "Third day1"]))
with(vos, shapiro.test(Osmolality[Stage == "Second day3"]))
with(vos, shapiro.test(Osmolality[Stage == "Third day2"]))

wilctest <- compare_means(Osmolality~Stage, vos,method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL)
write.csv(wilctest, 'wilctestverification_alldata.csv')

#Kruskal wallis test for original data
kruskal.test(Osmolality ~ Stage, data = vos) #significant

#probably not needed
ttest <- compare_means(Osmolality~Stage, vos,method = "t.test", paired = FALSE,
                          group.by = NULL, ref.group = NULL)
write.csv(ttest, 'ttestverification_alldata.csv')

#Summary stats of all data
vd1s<-group_by(vos, Stage) %>%
  summarise(
    count= n(),
    mean= mean(Osmolality, na.rm= TRUE),
    sd= sd(Osmolality, na.rm= TRUE),
    median = median(Osmolality, na.rm = TRUE),
    IQR = IQR(Osmolality, na.rm = TRUE)
  )
vd1s


#box and dot with pvalues for wilcoxon's test
#my_comparisonsv <- list( c("Second day3", "Third day1"),c("Second day3","Fresh molt"),c("Second day3","Second day2"))

bp <- ggboxplot(vos, x = "Stage", y = "Osmolality",
               fill = "Stage", palette = "Greys",size= 1.0)+geom_jitter(data=vos, mapping=aes(x=Stage, y=Osmolality),shape =16, size=3.5,color="darkgreen", position=position_jitter(width=0.2, height=0))+
               #stat_summary(fun= mean, geom="point", shape=17, size=3, color="green", fill="white")+ xlab ("Instar stage")+ ylab("Osmolality (mOsm)") + 
               #stat_compare_means(method= "wilcox.test",exact = FALSE,comparisons = my_comparisonsv,size=3.0, label.y = c(470, 450, 440))+
               theme(text = element_text(size = 20))+
               scale_x_discrete(limits=c("Second day1","Second day2", "Second day3", "Fresh molt","Third day1", "Third day2"))+
               scale_y_continuous(breaks=seq(0,650,by=100),limits = c(0,650))
bp



#defining the graph variables
iro <- read.csv("irosm.csv")
iro
inr <- ggboxplot(iro, x = "Stage", y = "Osmolality",
                fill = "Stage",size= 0.8,ylim=c(50,500))+geom_jitter(data=iro, mapping=aes(x=Stage, y=Osmolality),shape =16, size=3.5, position=position_jitter(width=0.2, height=0))+
  stat_summary(fun= mean, geom="point", size=3, color="blue")+ stat_summary(fun.data = mean_se,geom = "errorbar",size=1)+
  #stat_compare_means(method= "wilcox.test",exact = FALSE,comparisons = my_comparisonsv,size=3.0, label.y = c(470, 450, 440))+
  theme(text = element_text(size = 20))+
  scale_x_discrete(limits=c("100% Insect Ringers","50% Insect Ringers"))+scale_color_manual(values= c("blue","deepskyblue3"))
inr

#Plotting mean and se for osmotic solutions
#Summary stats
ird1s<-group_by(iro, Stage) %>%
  summarise(
    count= n(),
    mean= mean(Osmolality, na.rm= TRUE),
    sd= sd(Osmolality, na.rm= TRUE),
    se=se(Osmolality,na.rm=TRUE),
    median = median(Osmolality, na.rm = TRUE),
    IQR = IQR(Osmolality, na.rm = TRUE)
  )
ird1s

#plotting mean and SE
ggplot(ird1s, aes(x=Stage, y=mean,color=Stage))+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
  geom_point()+theme(text = element_text(size = 20),legend.position = "none")+
  scale_x_discrete(limits=c("100% Insect Ringers","50% Insect Ringers"))+scale_color_manual(values= c("blue","deepskyblue3"))+
  scale_y_continuous(breaks=seq(0,650,by=100),limits = c(0,650))

#box plot only for figure 3 panel B
#my_comparisonsv <- list( c("Second day3", "Third day1"),c("Second day3","Fresh molt"),c("Second day3","Second day2"))

bp2 <- ggboxplot(vos, x = "Stage", y = "Osmolality",
                fill = "Stage", palette = "Greys",size= 1.0,ylim=c(50,500))+
  theme(text = element_text(size = 20))+scale_x_discrete(limits=c("Fresh molt","Third day1", "Third day2"))
bp2


#
#
#
#
#Rev 2 Amartya's hemolymph collection from third instars in hyperosmotic solution. n= 14. Meausrements by Josh
revo <- read.csv("revos.csv")
revo

#QQplot
#original data
ggqqplot(revo, x = "Osmolality",
         color = "Group", facet.by = "Group",
         ggtheme = theme_pubclean())+facet_grid(. ~ Group)


bprev <- ggboxplot(revo, x = "Group", y = "Osmolality",
                fill = "Group", palette = "Greys",size= 1.0)+geom_jitter(data=revo, mapping=aes(x=Group, y=Osmolality),shape =16, size=3,color="darkgreen", position=position_jitter(width=0.3, height=0))+
  #stat_summary(fun= mean, geom="point", shape=17, size=3, color="green", fill="white")+ xlab ("Instar stage")+ ylab("Osmolality (mOsm)") + 
  #stat_compare_means(method= "wilcox.test",exact = FALSE,comparisons = my_comparisonsv,size=3.0, label.y = c(470, 450, 440))+
  theme(text = element_text(size = 20))+
  scale_x_discrete(limits=c("Control","Hyperosmotic"))+
  scale_y_continuous(breaks=seq(0,650,by=100),limits = c(0,650))
bprev

wilctestrev <- compare_means(Osmolality~Group, revo,method = "wilcox.test", exact = TRUE,
                          group.by = NULL, ref.group = NULL)
write.csv(wilctestrev, 'wilctestverification_alldata.csv')


#
#
#
#ALL OSMOLALITY
revoall <- read.csv("osm_rev.csv")
revoall
bprevall <- ggboxplot(revo, x = "Stage", y = "Osmolality",
                   fill = "Stage", palette = "Greys",size= 1.0)+geom_jitter(data=revo, mapping=aes(x=Stage, y=Osmolality),shape =16, size=3,color="darkgreen", position=position_jitter(width=0.3, height=0))+
  #stat_summary(fun= mean, geom="point", shape=17, size=3, color="green", fill="white")+ xlab ("Instar stage")+ ylab("Osmolality (mOsm)") + 
  #stat_compare_means(method= "wilcox.test",exact = FALSE,comparisons = my_comparisonsv,size=3.0, label.y = c(470, 450, 440))+
  theme(text = element_text(size = 20))+
  scale_x_discrete(limits=c("Second day1","Second day2", "Second day3", "Fresh molt third","Third day1", "Third day2","Third day 4","Third day 5"))+
  scale_y_continuous(breaks=c(0,100, 200, 300, 400, 500, 600, 650))
bprevall
