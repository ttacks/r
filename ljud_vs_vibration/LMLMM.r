#Ljud vs. Vibration-experiment


rm(list=ls())#ta bort allt skr�p
#Scriptens och analysernas hemmap
hMapp <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration")

#Bildmapp
bildDir <- file.path(Sys.getenv("HOME"), "GitHub", "r", "ljud_vs_vibration", "img")
#data_mapp
dDir <- file.path(Sys.getenv("HOME"), "My Box Files", "Data", "The_Useful_Files" ,"ljud_vs_vib")
#ladda in r�data
setwd(dDir); rawData = read.csv("rawData.csv", sep=";") 
setwd(hMapp)#Tillbaka till scriptets mapp
#Skapar ny data.frame med det som ska ing� i analysen
if(file.exists("dF0.rda")) load("dF0.rDa") else {dF0 <- subset(rawData, select = c( "Sub_id", "TrialNumber", 
                                                                                   "Block", "BlockModality", 
                                                                                   "Condition", "TrialType",
                                   "Modality", "Accuracy", "RT", "Condition2", "IntraBlock.NovelNumber"));
                                   save(dF0,file="dF0.Rda")}
#Tillbaka till scriptets mapp
setwd(hMapp)