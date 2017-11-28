# Function to normalize the counts in the form of a vector v, according to the cell number in matrix form
# author: vyepez

get_correction_factor = function(matrix, NT){
  
  # Force lower and upper limits for cell number
  matrix[matrix < CELL_NUMBER_MIN] <- NA
  # matrix[matrix > CELL_NUMBER_MAX] <- NA
  
  plate_av = mean(matrix, na.rm = T)
  correction_factor = plate_av / matrix
  correction_factor = as.vector( t(correction_factor) )
  
  cf = numeric()
  for(i in 1:96){
    cf = c( cf, rep(correction_factor[i], NT) )
  }
  return(cf)
}