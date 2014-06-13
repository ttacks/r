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
  ggsave(imageFile, width=15, dpi=72)	
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

#Plot för att plotta för varje block (spec för habitueringsstudien)
habPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(dataCT.pt, Condition == sel_name)         
  
  ggobj = ggplot(data = dum, aes(RT))  + geom_histogram(aes(y=..density..)) + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    stat_function(fun=dnorm, args = list(mean = mean(dum$RT, na.rm=TRUE), sd = sd(dum$RT, na.rm =TRUE))) +
    #Får bort det under
    scale_y_continuous(expand = c(0,0)) +
     ggtitle(paste("Histograms", sel_name, sep= " ")) #+
    #facet_wrap(~BlockModality)
  print(ggobj)
  filename = paste("1_Histogram_Modality",sel_name, ".png", sep="")
  saveInImageDirectory(filename)
}
lapply(unique(dataCT.pt$Condition), habPlot)
################################################################################
#Försöker extrahera residualer för att kontrollera normalfördelning
model1 <- lme(y1 ~  block  + y2 + y3 + y4,data=aa, random= list(id = pdSymm(~fblock-1)), method  = 'ML')
#summary(model1) 
aa$resid <- residuals(model1)
require(reshape2)
plotDF <- melt(aa[,c("y2", "y3","y4", "resid")], id="resid")
ggplot(plotDF, aes(x=value, y=resid)) + 
  geom_point() + facet_wrap(~ variable)
#########

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

#qqplots kan man änvda sig av qplot(sample = data.frame$kolumn.name, stat ="qq")#density plot

density <- ggplot(data, aes(RT))
density + geom_density() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~C + Block) 
  
#Spara filen. Behöver utöka
#ggsave(filename = "histograms.png")
#Scatterplotta! method="lm" betyder att det är för att passa en linjär model (lm)
scatter <- ggplot(dataCT, aes(BlockModality, RT, colour = Condition))
scatter + geom_point(position="dodge") + geom_smooth(data = dataCT, method="lm", fill = dataCT$Condition)  + labs(x ="Block", y="Mean RT (ms)", colour = "Condition") + 
  #gör 4 scatterplots, en per condition!
  facet_wrap(~Modality)

#Skapar en graph mha ggplot. Data är data filen, sedan i aes-funktionen så är Block x-axel, RT y axel, färgkodning efter
#C (condition)
mGraph <- ggplot(data, aes(Block, RT, colour=C)) + labs(title="RT")
mGraph + geom_bar() + geom_point()

#Boxplots
boxP <- ggplot(dataCT, aes(BlockModality, RT, group=BlockModality, fill=BlockModality))  
boxP + geom_boxplot() + facet_wrap(~Condition)

#Bar charts!
bar <- ggplot(dataCT.pt, aes(BlockModality, RT, fill=Condition))
bar +
  #position = "dodge" gör så att jag får skilda bars för varje condition
  stat_summary(data=datacT.pt, fun.y = mean, geom="bar", color=Condition, group=Condition, position="dodge") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +

 facet_wrap(~Modality) 

#Linjeplot för habituering.

linje <- ggplot(data, aes(Block, RT, fill=C, color=C, shape=C))
linje + stat_summary(fun.y = mean, geom ="point", aes(fill=C)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() 
        ,legend.background = element_blank()
        ,legend.key = element_blank()) +
  ylab("mean RT (ms)") +

  stat_summary(fun.y = mean, geom = "line", aes(fill=C))


#2 Kolla statistiska antaganden

#Normalfördelning
#1 habDesc är en funktion som genererar tabeller för varje
#2 QQplots kan användas för att tolka shapiro-wilks (ett av värdena i tabellen)

#describe och stat.desc från psych och pastecs respektive kan ge en olika deskriptiv data,
#cbind verkar vara en bra funktion. I kombination med ovan nämnda funktioner kan man kolla flera variabler
#Värden borde vara så nära noll som möjligt annars är de Skewed (kom ihåg vad Johan sa att har man nog många obs. kommer det all
#id se skevt ut). Vidare kan man konvertera värdena för skew och kurtosis till z-poäng. I små stickprov (vad det nu är)
#är det ok att kolla efter värden över 1.96 men i stora bör man ändra kriterium till 2.58.

#round kan användas för att avrunda siffror så vi år mindre decimaler! det anger man "digits"
#round(stat.desc(data.frame[, c("row1", "row2", "etc")], basic = FALSE, norm =TRUE), digits=3)
#1
habDesc<-function(sel_name) { #Denna funktion kör per unik i kolumnn "C". Detta kan ändras i lapply under
  require(pastecs)
  require(OIdata)
  require(gridExtra)
  require(ggplot2)
  output<- matrix(ncol=13, nrow=blocks)
  r.names <- matrix(ncol=1, nrow=blocks)
  for (i in 1:blocks){
    assign(paste("subset", i, sep = ""),subset(dataCT, BlockModality == i)) 

    e1 <- get(paste("subset", i, sep=""))
    dum = subset(e1, Condition == sel_name) 
    s.test <- shapiro.test(dum$RT)
    print(s.test)
    tab <- round(stat.desc(dum$RT, basic= FALSE, norm=TRUE), digits = 2)
    c.names <- t(names(tab))
    r.names[i,] <- paste(sel_name, i, sep="")
    
    output[i,] <- t(tab)
  }
  r.names <- t(r.names)
  colnames(output) <- c.names #Kolumnerna får rubriker
  rownames(output) <- r.names #Raderna får rubriker
  xyTable <- output
  qplot(1:10, 1:10, geom = "blank") + 
    theme_bw() +
    theme(line = element_blank(),
          text = element_blank()) +
    annotation_custom(grob = tableGrob(xyTable,
                                       # change font sizes:
                                       gpar.coltext = gpar(cex = 1.2),
                                       gpar.rowtext = gpar(cex = 1.2)),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
  
  filename = paste("Descript_",sel_name, ".png", sep="")
  saveInImageDirectory(filename)
  
}
blocks <- length(unique(dataCT$BlockModality))
ads <- sapply(unique(dataCT$Condition), habDesc, simplify="matrix")

#2 QQPlot #### For hab_ Vanlig rt
hqqPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(dataCT.pt, Condition == sel_name)    
  filename = paste("1_QQ_",sel_name, ".png", sep="")
  iFile <- file.path(imageDirectory, filename)
  #sparar filen innan sas.
  png(filename = iFile, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png")) 
  #par skapar här de fyra graferna (en per block) i en bild
  op <- par(mfrow=c(2,2))
  for (i in 1:blocks){
    #subsettar block per subset1, subset2, etc.
    assign(paste("subset", i, sep = ""),subset(dataCT.pt, BlockModality == i)) 
    
    e1 <- get(paste("subset", i, sep=""))
    dum = subset(e1, Condition == sel_name) 
  
    qqnorm(dum$RT, xlab=paste(sel_name, i, sep=""))
    qqline(dum$RT)
  }
  par(op)
  graphics.off()
  rm(i)    
}
blocks <- length(unique(dataCT.pt$BlockModality))
lapply(unique(dataCT.pt$Condition), hqqPlot)

#Trimmad lång RT
hqqPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(dataCT.pt, Condition == sel_name)    
  dum <- dum[dum$RT<=1000, 1:27]
  filename = paste("1_Trimmed_QQ_",sel_name, ".png", sep="")
  iFile <- file.path(imageDirectory, filename)
  #sparar filen innan sas.
  png(filename = iFile, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png")) 
  #par skapar här de fyra graferna (en per block) i en bild
  op <- par(mfrow=c(2,2))
  for (i in 1:blocks){
    #subsettar block per subset1, subset2, etc.
    assign(paste("subset", i, sep = ""),subset(dataCT.pt, BlockModality == i)) 
    
    e1 <- get(paste("subset", i, sep=""))
    dum = subset(e1, Condition == sel_name) 
    
    qqnorm(dum$RT, xlab=paste(sel_name, i, sep=""))
    qqline(dum$RT)
  }
  par(op)
  graphics.off()
  rm(i)    
}
blocks <- length(unique(dataCT.pt$BlockModality))
lapply(unique(dataCT.pt$Condition), hqqPlot)

#logtransformerade
hqqPlot<-function(sel_name) {
  require(ggplot2)
  dum = subset(dataCT.pt, Condition == sel_name)    

  filename = paste("1_Logtransformed_QQ_",sel_name, ".png", sep="")
  iFile <- file.path(imageDirectory, filename)
  #sparar filen innan sas.
  png(filename = iFile, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png")) 
  #par skapar här de fyra graferna (en per block) i en bild
  op <- par(mfrow=c(2,2))
  for (i in 1:blocks){
    #subsettar block per subset1, subset2, etc.
    assign(paste("subset", i, sep = ""),subset(dataCT.pt, BlockModality == i)) 
    
    e1 <- get(paste("subset", i, sep=""))
    dum = subset(e1, Condition == sel_name) 
    
    qqnorm(dum$lrt, xlab=paste(sel_name, i, sep=""))
    qqline(dum$lrt)
  }
  par(op)
  graphics.off()
  rm(i)    
}
blocks <- length(unique(dataCT.pt$BlockModality))
lapply(unique(dataCT.pt$Condition), hqqPlot)

#########################################################


#Testa homogeneity of variance
#Levene's test. Testar nolhyptesen att variansen i olika grupper är samma.
h.levenesT<-function(sel_name){
    dum = subset(dataCT, Block == sel_name) 
    leveneTest(dum$RT, dum$C, center=mean)
}
levs <- lapply(unique(dataCT$BlockModality), h.levenesT)

###Ifall det går åt helvete:
# ... alternative DVs
#Olika transformeringar... Kör ovan test igen...
dataCT.pt$srt <- dataCT.pt$RT/1000
dataCT.pt$lrt <- log(dataCT.pt$RT)
dataCT.pt$qrt <- sqrt(dataCT.pt$RT)         # close lambda, from boxcox: 0.424 ~ 0.5 ~ sqrt(rt)
dataCT.pt$prt <- dataCT.pt$RT^(lambda)      # exact lambda 


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

#mer Descrp.
#Skapa tabell för M och Mdn. Bör subsetta modality och göre per modality
#Därav .vib. Kan skrivas om till funktion I guess!


flst.vib <- list(dBW.vib$Blocks, dBW.vib$TrialType)
(tN <- tapply(dBW.vib$RT, flst.vib, FUN = function(x) length(x[!is.na(x)])))
tMn.vib <- tapply(dBW.vib$RT, flst.vib, FUN = mean)
tMd.vib <- tapply(dBW.vib$RT, flst.vib, FUN = median)
colnames( (res.vib <- c(tN, tMn.vib, tMd.vib)))
colnames( (res.vib <- cbind(tN, tMn.vib, tMd.vib)))
nms1 <- rep(c("S", "D"), 3)
nms2 <- rep(c("n", "Mean", "Mdn"), rep(2,3))

colnames(res.vib) <- paste(nms1, nms2, sep = ":")
res


#Creating my models now...
