#Funktioner f�r att trimma och transformera data,
# 1.) Skapa data.frame f�r enbart korrekta responser

#Funktion f�r att skapa en ny data.frame f�r korrekta responser

rmInc<-function(dataF, rem, colU){
  
  dataF <- dataF[-grep(rem, dataF$colU),]
  
  return(dataF)
}

rmTT<-function(dataF, rem){
  
  dataF <- dataF[-grep(rem, dataF$TrialType),]
  
  return(dataF)
}

rmLRT <- function(dataF){
  dataF <-s ubset(dataC, RT >= 200)
  return(dataF)
  
}