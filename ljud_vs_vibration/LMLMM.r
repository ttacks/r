#Ljud vs. Vibration-experiment
#Skapar f�rst en himla massa data.frames. Dessa kan kanske flyttas till d�r de sen kommer anv�ndas
#De kan tas bort efter de anv�nts!
rm(list=ls())#ta bort allt skr�p
#Scriptens och analysernas hemmap
hMapp <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration")

#Bildmapp
bildDir <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration", "img")
#data_mapp
dDir <- file.path(Sys.getenv("HOME"), "My Box Files", "Data", "The_Useful_Files" ,"ljud_vs_vib")
#ladda in r�data
setwd(dDir); if(file.exists("rawData.csv") rawData = read.csv("rawData.csv", sep=";") else print("Raw data file not available")
setwd(hMapp)#Tillbaka till scriptets mapp
#Skapar ny data.frame med det som ska ing� i analysen
if(file.exists("dF0.rda")) load("dF0.rDa") else {dF0 <- subset(rawData, select = c( "Sub_id", "TrialNumber", 
                                                                                   "Block", "BlockModality", 
                                                                                   "Condition", "TrialType",
                                   "Modality", "Accuracy", "RT", "Condition2", "IntraBlock.NovelNumber"));
                                   save(dF0,file="dF0.Rda")}
#Olika transformeringar.
dF0$srt <- dF0$RT/1000
dF0$lrt <- log(dF0$RT)
dF0$qrt <- sqrt(dF0$RT)         # close lambda, from boxcox: 0.424 ~ 0.5 ~ sqrt(rt)
dF0.hit <- dF0[dF0$Accuracy==1,] #DF med bara hits
#Skapa 2 data.frames (En f�r enbart hits och en f�r ACC ber�kn) d�r post-deviant-standard �r borttagen:
dF1.ACC  <- dF0[-grep("Standard-post-deviant",dF0$TrialType),]; dF1.ACC$TrialType <- factor(dF1.ACC$TrialType); 
dF1.ACC$Condition2 <- factor(dF1.ACC$Condition2); str(dF1.ACC)

dF1 <- dF1.ACC[dF1.ACC$Accuracy==1,] #Post-dev borttagen. Viktigast f�r huvudanalys.

#Lite deskriptiv data:
n <- length(unique(rawData$Sub_id))
mAge <- mean(rawData$Age)