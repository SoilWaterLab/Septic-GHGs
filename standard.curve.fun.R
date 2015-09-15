standard.curve.fun <-function(gas, print=FALSE){
  # This is the septic standard curve function
  # Note that this version has been edited to output a constant standard curve, due to leaking standard bags.  
  # The slope and intercept for the standard curves are based on an average of eleven standard
  # sets run after the standard bags were refilled on 8/8/2015
  # Last edited 08/27/2015 by Allison Truhlar
  
  
  # Set slope and intercept based on gas type
  if (gas=="N2O"){
    intercept <- -0.08383 	
    slope <- 0.008121
  } else if (gas=="CH4"){
    intercept <- -0.41026 	
    slope <- 0.714992
  } else if (gas=="CO2"){
    intercept <- -246.326
    slope <- 0.710008
  }
  
  # Output the intercept and slope for each standard curve
  standard_coeffs <- data.frame(Intercept=intercept,Slope=slope)
  
}