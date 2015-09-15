gas.directory.fun <-function(dir, gas, date, name, print=FALSE){
  # This function creates an array of file names that will be used to open appropriate csv
  # data files and to save your generated graphs
  
  # Last edited on 08/27/2015 by Allison Truhlar
  
  # This update saved all the output files with "fixed-coeffs" in front of the usual descriptive name, to indicate
  # that all the outputs have been processed using fixed standard curves to account for leaking of the standard bags
  
  OS <- 'W' #readline(prompt="Enter W for Windows or M for Mac: ")
  
  if (OS == 'W'){
    dir <- gsub("\\\\","/",dir, fixed=TRUE)
  }
  
  # create the directories for fixed coeff files, if needed
  if (dir.exists(paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs",sep=""))==FALSE) {
    dir.create(paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs",sep=""))}
    
  sample_directory <- rep(NA_character_, 2)
  
  sample_directory <- c(paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,"_standard.csv", sep="", collapse=NULL), 
    paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_chamber-heights.csv", sep="", collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/2015",date,"_septic-",name,"_",gas,".csv", sep="", collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs/2015",date,"_septic-",name,"_cap-velocity.csv", sep="", collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs/2015",date,"_septic-",name,"_",gas,"_fixed-coeffs-graph.png", sep="", collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs/2015",date,"_septic-",name,"_",gas,"_fixed-coeffs-processed.csv", sep="", collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs/2015",date,"_septic-",name,"_",gas,"_fixed-coeffs-scaled_flux.csv",sep="",collapse=NULL),
    paste(dir,"Data-",name,"/2015",date,"/Fixed-coeffs/2015",date,"_septic-",name,"_",gas,"_fixed-coeffs-conc.csv",sep="",collapse=NULL))

  
}
