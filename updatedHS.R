setwd("/Users/rthrs/OneDrive - University of Cincinnati/PhD Data/Aim2/huntingexp")

#Horizontal
hz <- read.csv("hshrz.csv")
hz

#defining the graph variables
hzp <- ggplot(hz, aes(x = Group, y = Hunting.success))

#violin and box plot
finalplothz <- hzp +geom_boxplot(
    aes(fill = Group), width = 0.7,
    position = position_dodge(0.9)
  )+ geom_dotplot(
    aes(fill= NULL), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.05))+
  scale_x_discrete(limits=c("Control", "Hyperosmotic"))+scale_fill_manual(values=c("red", "blue"))
#+scale_x_discrete(limits=c("a", "aa", "b","bb","c","cc","d","dd"))

#removing background panels
finalplothz+ theme( axis.line = element_line(colour = "black"))+ theme(text = element_text(size = 20))+ ylim(0,100)+ylab ("% Hunting success") + xlab (NULL)

compare_means(Hunting.success~Group, hz, method = "wilcox.test", exact= FALSE, paired = FALSE,
              group.by = NULL, ref.group = NULL)

#means
d1h<-group_by(hz, Group) %>%
  summarise(
    count= n(),
    mean= mean(Hunting.success, na.rm= TRUE),
    sd= sd(Hunting.success, na.rm= TRUE)
  )
d1h

#QQplot
#original data
ggqqplot(hz, x = "Hunting.success",
         color = "Group",ggtheme = theme_pubclean())

#Testing normal distribution
with(hz, shapiro.test(Hunting.success[Group == "Control"])) #Normal
with(hz, shapiro.test(Hunting.success[Group == "Hyperosmotic"])) #Normal

#Ftest
var.test(Hunting.success ~ Group, hz, 
         alternative = "two.sided") #Not variable

#t.test
compare_means(Hunting.success ~ Group, hz, method = "t.test", paired = FALSE,
              group.by = NULL, ref.group = NULL) #USED FOR THE PAPER
#wilcoxon's test
compare_means(Hunting.success ~ Group, hz, method = "wilcox.test", exact = TRUE,
              group.by = NULL, ref.group = NULL) #NOT USED


#Vertical
hvert <- read.csv("hsvrt.csv")
hvert

#defining the graph variables
hvp <- ggplot(hvert, aes(x = Group, y = Hunting.success))

#violin and box plot
finalplothv <- hvp + geom_boxplot(
    aes(fill = Group), width = 0.7,
    position = position_dodge(0.9)
  )+ geom_dotplot(
    aes(fill= NULL), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.05))+
  scale_x_discrete(limits=c("Control", "InsectRingers"))+scale_fill_manual(values=c("red", "blue"))
#+scale_x_discrete(limits=c("a", "aa", "b","bb","c","cc","d","dd"))

#removing background panels
finalplothv+  theme( axis.line = element_line(colour = "black"))+ theme(text = element_text(size = 20))+ ylim(0,100)+ylab ("% Hunting success") + xlab (NULL)

compare_means(Hunting.success~Group, hvert, method = "wilcox.test", exact= FALSE, paired = FALSE,
              group.by = NULL, ref.group = NULL)


####latency
#horizontal
lhz <- read.csv("lathz.csv")
lhz

#defining the graph variables
lhzp <- ggplot(lhz, aes(x = group, y = latencymin))

#violin and box plot
finalplotlhz <- lhzp +geom_boxplot(
    aes(fill = group), width = 0.7,
    position = position_dodge(0.9)
  )+ geom_dotplot(
    aes(fill= NULL), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.05))+
  scale_x_discrete(limits=c("Control", "Hyperosmotic"))+scale_fill_manual(values=c("red", "blue"))
#+scale_x_discrete(limits=c("a", "aa", "b","bb","c","cc","d","dd"))

#removing background panels
finalplotlhz+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size = 20))+ylim(0,25) +ylab ("Latency(min)") + xlab (NULL)

compare_means(latencymin~group, lhz, method = "wilcox.test", exact= FALSE, paired = FALSE,
              group.by = NULL, ref.group = NULL)

##vertical
vhz <- read.csv("latvrt.csv")
vhz

#defining the graph variables
vhzp <- ggplot(vhz, aes(x = group, y = latencymin))

#violin and box plot
finalplotvhz <- vhzp + geom_boxplot(
    aes(fill = group), width = 0.7,
    position = position_dodge(0.9)
  )+ geom_dotplot(
    aes(fill= NULL), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.05))+
  scale_x_discrete(limits=c("Control", "InsectRingers"))+scale_fill_manual(values=c("red", "blue"))
#+scale_x_discrete(limits=c("a", "aa", "b","bb","c","cc","d","dd"))

#removing background panels
finalplotvhz+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size = 20))+scale_y_continuous(breaks=seq(0,30,by=5.0),limits = c(0,30)) +ylab ("Latency(min)") + xlab (NULL)

compare_means(latencymin~group, vhz, method = "wilcox.test", exact= FALSE, paired = FALSE,
              group.by = NULL, ref.group = NULL)
