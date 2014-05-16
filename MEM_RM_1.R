#Repeated measures the multilevel (mixed effects) modeling approach


#B�rjar med att utforska data. 
#Att l�gga till:
#     1. Normalf�rdelnings-funktion
#        Denna b�r antagligen unders�ka huruvida varje variabel tillh�r en normalf�rdelning eller ej

#   !!!faceting kan anv�ndas f�r att dela upp plottar i flera
#   !!!Position = "x" kan �ven anv�ndas f�r att fixa s� att det inte �verlappar saker
#   !!!B�rjar att anv�nda mig av ggplot2

#A simple approach with loops would be
#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

#setwd("~/Documents/Academic/Data/DSU_R/Chapter 04 (Graphs) R")
imageDirectory<-"~/R/Multi-level modeling R (MEM, GMEM)"

imageDirectory<-file.path(Sys.getenv("HOME"), "R", "Multi-level modeling R (MEM, GMEM)")
saveInImageDirectory<-function(filename){
  imageFile <- file.path(imageDirectory, filename)
  ggsave(imageFile)	
}

#Doing histograms. En f�r varje condition.
doPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(data, C == sel_name)
  
  ggobj = ggplot(data = dum, aes(RT))  + geom_histogram(aes(y=..density..)) + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    stat_function(fun=dnorm, args = list(mean = mean(dum$RT, na.rm=TRUE), sd = sd(dum$RT, na.rm =TRUE))) +
    #F�r bort det under
    scale_y_continuous(expand = c(0,0)) 
  print(ggobj)
  filename = sprintf("%s.png", sel_name)
  saveInImageDirectory(filename)
}
lapply(unique(data$C), doPlot)
#histogram, denna funktion m�ste skrivas om lite f�r att kunna ta "allm�n" oddball-design!
cHisto <-function(data.frame, var){
  require(ggplot2)
  mHisto <- ggplot(data.frame, aes(RT))
  mHisto + geom_histogram(aes(y=..density..)) +
    #Denna fungerar inte helt f�r den tar nog medel av all RT
    stat_function(fun=dnorm, args = list(mean = mean(data.frame$RT, na.rm=TRUE), sd = sd(data.frame$RT, na.rm =TRUE))) +
    #Svartvit bakgrund
    theme_bw() +
    #Ta bort grid. Just nu ser det inte j�ttebra ut d� noll inte startar p� botten av graf.
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #En graph f�r varje block och condition. vore SNYGGARE att ist�llet g�ra EN egen plot f�r varje condition och block.
    facet_wrap(~C + Block) 
}
#Q-Q plot
qqplot <-
#density plot
density <- ggplot(data, aes(RT))
density + geom_density() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~C + Block) 
  
#Spara filen. Beh�ver ut�ka
#ggsave(filename = "histograms.png")
#Scatterplotta! method="lm" betyder att det �r f�r att passa en linj�r model (lm)
scatter <- ggplot(data, aes(Block, RT, group=C, colour = C))
scatter + geom_point(position="dodge") + geom_smooth(method="lm", fill = C)  + labs(x ="Block", y="Mean RT (ms)", colour = "Condition") + 
  #g�r 4 scatterplots, en per condition!
  facet_wrap(~C)

#Skapar en graph mha ggplot. Data �r data filen, sedan i aes-funktionen s� �r Block x-axel, RT y axel, f�rgkodning efter
#C (condition)
mGraph <- ggplot(data, aes(Block, RT, colour=C)) + labs(title="RT")
mGraph + geom_bar() + geom_point()

#Boxplots
boxP <- ggplot(data, aes(Block, RT, group=Block, fill=Block))  
boxP + geom_boxplot() + facet_wrap(~C)

#Bar charts!
bar <- ggplot(data, aes(Block, RT, fill=C))
bar +
  #position = "dodge" g�r s� att jag f�r skilda bars f�r varje condition
  stat_summary(fun.y = mean, geom="bar", color=C, group=C, position="dodge") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#+ facet_wrap(~C) 

#Linjeplot

linje <- ggplot(data, aes(Block, RT, fill=C, color=C, shape=C))
linje + stat_summary(fun.y = mean, geom ="point", aes(fill=C)) +
  stat_summary(fun.y = mean, geom = "line", aes(fill=C))


#2 Kolla statistiska antaganden

#Normalf�rdelning
require(pastecs)
require(psych)
require(rcmdr)

#Andy Fields "outlier summary"
outlierSummary<-function(variable, digits = 2){
  
  zvariable<-(variable-mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
  
  outlier95<-abs(zvariable) >= 1.96
  outlier99<-abs(zvariable) >= 2.58
  outlier999<-abs(zvariable) >= 3.29
  
  ncases<-length(na.omit(zvariable))
  
  percent95<-round(100*length(subset(outlier95, outlier95 == TRUE))/ncases, digits)
  percent99<-round(100*length(subset(outlier99, outlier99 == TRUE))/ncases, digits)
  percent999<-round(100*length(subset(outlier999, outlier999 == TRUE))/ncases, digits)
  
  cat("Absolute z-score greater than 1.96 = ", percent95, "%", "\n")
  cat("Absolute z-score greater than 2.58 = ",  percent99, "%", "\n")
  cat("Absolute z-score greater than 3.29 = ",  percent999, "%", "\n")
}