#Ljud vs. Vibration-experiment
#Skapar först en himla massa data.frames. Dessa kan kanske flyttas till där de sen kommer användas
#De kan tas bort efter de använts!
rm(list=ls())#ta bort allt skräp
#Scriptens och analysernas hemmap
hMapp <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration")

#Bildmapp
bildDir <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration", "img")
#data_mapp
dDir <- file.path(Sys.getenv("HOME"), "My Box Files", "Data", "The_Useful_Files" ,"ljud_vs_vib")
#ladda in rådata
setwd(dDir); if(file.exists("rawData.csv") rawData = read.csv("rawData.csv", sep=";") else print("Raw data file not available")
setwd(hMapp)#Tillbaka till scriptets mapp
#Skapar ny data.frame med det som ska ingå i analysen
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
#Skapa 2 data.frames (En för enbart hits och en för ACC beräkn) där post-deviant-standard är borttagen:
dF1.ACC  <- dF0[-grep("Standard-post-deviant",dF0$TrialType),]; dF1.ACC$TrialType <- factor(dF1.ACC$TrialType); 
dF1.ACC$Condition2 <- factor(dF1.ACC$Condition2); str(dF1.ACC)

dF1 <- dF1.ACC[dF1.ACC$Accuracy==1,] #Post-dev borttagen. Viktigast för huvudanalys.

#Lite deskriptiv data:
n <- length(unique(rawData$Sub_id))
mAge <- mean(rawData$Age)