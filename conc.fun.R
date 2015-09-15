conc.fun <-function(sample_data, standard_coeffs, chamber_volumes, directory, gas, print=FALSE){
  #Note: edited on 8/28/15 to include a fixed standard coefficient slope and intercept

collar_surface_area <- 0.064 #meters

std_curve_nums <- unique(standard_coeffs$Std_Curve_Num)

for(j in 1:nrow(sample_data))	{
  
  sample_data$Concentration[j] <- sapply(sample_data$Area[j], function(x) x*standard_coeffs$Slope + standard_coeffs$Intercept)
  
  if (is.na(sample_data$Concentration[j])){
    sample_data$Concentration[j]=NA
  }else if(sample_data$Concentration[j]<0){
    sample_data$Concentration[j]=0
  } 
}

sample_data$Time <- as.numeric(sample_data$Time)
sample_data$Time_min <- sapply(sample_data$Time, function(x) x/60)

conc_data <- sample_data
}