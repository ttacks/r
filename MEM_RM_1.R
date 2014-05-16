#Repeated measures the multilevel (mixed effects) modeling approach


#Börjar med att utforska data. 
#Att lägga till:
#     1. Normalfördelnings-funktion
#        Denna bör antagligen undersöka huruvida varje variabel tillhör en normalfördelning eller ej

#   !!!faceting kan användas för att dela upp plottar i flera
#   !!!Position = "x" kan även användas för att fixa så att det inte överlappar saker
#   !!!Börjar att använda mig av ggplot2

#A simple approach with loops would be
#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

#setwd("~/Documents/Academic/Data/DSU_R/Chapter 04 (Graphs) R")
imageDirectory<-"~/R/Multi-level modeling R (MEM, GMEM)"

imageDirectory<-file.path(Sys.getenv("HOME"), "R", "Multi-level modeling R (MEM, GMEM)")
saveInImageDirectory<-function(filename){
  imageFile <- file.path(imageDirectory, filename)
  ggsave(imageFile)	
}

#Doing histograms. En för varje condition.
doPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(data, C == sel_name)
  
  ggobj = ggplot(data = dum, aes(RT))  + geom_histogram(aes(y=..density..)) + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    stat_function(fun=dnorm, args = list(mean = mean(dum$RT, na.rm=TRUE), sd = sd(dum$RT, na.rm =TRUE))) +
    #Får bort det under
    scale_y_continuous(expand = c(0,0)) 
  print(ggobj)
  filename = sprintf("%s.png", sel_name)
  saveInImageDirectory(filename)
}
lapply(unique(data$C), doPlot)

model1 <- lme(y1 ~  block  + y2 + y3 + y4,data=aa, random= list(id = pdSymm(~fblock-1)), method  = 'ML')
#summary(model1)
aa$resid <- residuals(model1)
require(reshape2)
plotDF <- melt(aa[,c("y2", "y3","y4", "resid")], id="resid")
ggplot(plotDF, aes(x=value, y=resid)) + 
  geom_point() + facet_wrap(~ variable)
#histogram, denna funktion måste skrivas om lite för att kunna ta "allmän" oddball-design!
cHisto <-function(data.frame, var){
  require(ggplot2)
  mHisto <- ggplot(data.frame, aes(RT))
  mHisto + geom_histogram(aes(y=..density..)) +
    #Denna fungerar inte helt för den tar nog medel av all RT
    stat_function(fun=dnorm, args = list(mean = mean(data.frame$RT, na.rm=TRUE), sd = sd(data.frame$RT, na.rm =TRUE))) +
    #Svartvit bakgrund
    theme_bw() +
    #Ta bort grid. Just nu ser det inte jättebra ut då noll inte startar på botten av graf.
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #En graph för varje block och condition. vore SNYGGARE att istället göra EN egen plot för varje condition och block.
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
  
#Spara filen. Behöver utöka
#ggsave(filename = "histograms.png")
#Scatterplotta! method="lm" betyder att det är för att passa en linjär model (lm)
scatter <- ggplot(data, aes(Block, RT, group=C, colour = C))
scatter + geom_point(position="dodge") + geom_smooth(method="lm", fill = C)  + labs(x ="Block", y="Mean RT (ms)", colour = "Condition") + 
  #gör 4 scatterplots, en per condition!
  facet_wrap(~C)

#Skapar en graph mha ggplot. Data är data filen, sedan i aes-funktionen så är Block x-axel, RT y axel, färgkodning efter
#C (condition)
mGraph <- ggplot(data, aes(Block, RT, colour=C)) + labs(title="RT")
mGraph + geom_bar() + geom_point()

#Boxplots
boxP <- ggplot(data, aes(Block, RT, group=Block, fill=Block))  
boxP + geom_boxplot() + facet_wrap(~C)

#Bar charts!
bar <- ggplot(data, aes(Block, RT, fill=C))
bar +
  #position = "dodge" gör så att jag får skilda bars för varje condition
  stat_summary(fun.y = mean, geom="bar", color=C, group=C, position="dodge") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#+ facet_wrap(~C) 

#Linjeplot

linje <- ggplot(data, aes(Block, RT, fill=C, color=C, shape=C))
linje + stat_summary(fun.y = mean, geom ="point", aes(fill=C)) +
  stat_summary(fun.y = mean, geom = "line", aes(fill=C))


#2 Kolla statistiska antaganden

#Normalfördelning
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