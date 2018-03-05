# The corners of the Seahorse experiments were not always forced to be 0
# So, we have to remove those background values by computing the mean 
# of the 4 corners at each time point and then subtract that value from all
# wells at that time point
# author: vyepez

# NT: number of time points in the whole experiment (usually 12)

remove_mean_bg = function(v, NT){
  mean_corners = numeric(NT)
  
  # For each time point, get the mean of the corners (wells 1, 12, 85, 96)
  for(i in 1:NT){
    mean_corners[i] = mean( c(v[i], v[11*NT+i], v[84*NT+i], v[95*NT+i]) )
  }
  
  # For each time point and well, subtract the value with its mean
  for(i in 1:NT){
    for(j in 0:95){
      v[i+j*NT] = v[i+j*NT] - mean_corners[i]
      if(v[i+j*NT] <= 0 | v[i+j*NT] > OCR_MAX){ # Discard OCR<=0 & OCR>800 (OCR_MAX)
        v[i+j*NT] = NA 
      }
    }
    # assign NA to the corners
    v[i] = v[11*NT+i] = v[84*NT+i] = v[95*NT+i] = NA
  }
  
  return(v)
}
