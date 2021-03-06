
#Load libraries if just starting
library(reshape2)
library(plyr)

# Clear workspace
rm(list=ls())

#Set working R directory
#Keiran: "C://Users//Ross//Desktop//R_code//GC//"
#Allison: "C://Users//Owner//Dropbox//Cornell//Septic_2015//2015_data//"
dir <- "C://Users//Owner//Dropbox//Cornell//Septic_2015//2015_data//"
setwd(dir)


#Set working flux directory
#Keiran: "C://Users//Ross//Desktop//Septic 2015//Septic 2015//2015_data//Raw data"
#Keiran temporary: C://Users//Ross//Desktop//R_code//GC//Flux//"
#Allison: "C://Users//Owner//Dropbox//Cornell//Septic_2015//2015_data//Raw data//"
fdir <- "C://Users//Owner//Dropbox//Cornell//Septic_2015//2015_data//Raw data//"


#Load description file
flux_descript <- (read.csv(paste(fdir,"flux_descript.csv",sep="")))


gas <- 1 #sets first gas to n2o
i <- 1 #Index
r<- nrow(flux_descript)
while(i<=r){
  if(gas==1){gas_name <- "N2O"}
  if(gas==2){gas_name <- "CH4"}
  if(gas==3){gas_name <- "CO2"}
  date <- flux_descript$Date[i]
  owner <- flux_descript$Owner[i]
  
  #Open correct flux file
  #Note this is currently set to open the fixed standard coefficient data
  flux_directory <- (paste(dir,"Data-",owner,"//",date,"//Fixed-coeffs//",sep=""))
  flux_list <- data.frame(list.files(path=flux_directory, pattern=paste("*",gas_name,"_fixed-coeffs-scaled_flux.csv",sep="")))
  colnames(flux_list) <- "File"
  flux_file <- read.csv(paste(flux_directory,flux_list$File[1],sep=""))
  flux_file <- flux_file[c(2,3,4)]
  flux_file$Date <- rep(date,nrow(flux_file))
  flux_file$Owner <- rep(owner,nrow(flux_file))
  flux_file$Gas <- rep(gas_name,nrow(flux_file))
  flux_file <- flux_file[c(4,5,6,1,2,3)]
  if(exists("flux_file_old")==TRUE){flux_file <- rbind(flux_file_old,flux_file)}
  flux_file_old <- flux_file
  i <- i+1
  if(i>r&gas<3){gas<- gas+1}+{i<- 1}
}
write.csv(flux_file_old,row.names=FALSE,file=paste(dir,"flux_grab_output.csv", sep="")) 
