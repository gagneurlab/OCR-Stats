# Script with different useful functions

if.else = function(test, yes, no){
  if(test){ yes } else no
}

 
mean2 = function(x) mean(x, na.rm = T)
median2 = function(x) median(x, na.rm = T)
min2 = function(x) min(x, na.rm = T)
max2 = function(x) max(x, na.rm = T)

sum3 = function(x) if.else(sum(is.na(x)) == length(x), NA, sum(x, na.rm = T)) 
mean3 = function(x) if.else(sum(is.na(x)) == length(x), NA, mean(x, na.rm = T)) 
median3 = function(x) if.else(sum(is.na(x)) == length(x), NA, median(x, na.rm = T)) 
min3 = function(x) if.else(sum(is.na(x)) == length(x), NA, min(x, na.rm = T))
max3 = function(x) if.else(sum(is.na(x)) == length(x), NA, max(x, na.rm = T))

stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# Removes element(s) 'a' from vector 'v'
remove.element <- function(a, v){
  v = v[!v %in% a]
  return(v)
}

remove_na <- function(v){
  return(v[complete.cases(v)])
}

# If there's an NA value in the elements to be pasted, returns NA, else, returns pasted elements
paste_NA <- function(..., sep = ", ") {
  L <- list(...)
  l <- sum( sapply(L, function(x) is.na(x)) )
  ret = ifelse(l, NA, gsub(paste0("(^",sep,"|",sep,"$)"),"",
                           gsub(paste0(sep,sep), sep,
                                do.call(paste,c(L,list(sep=sep))))) )
  return(ret)
}


# Multi substitution
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  return(result)
}


# Sum of squares
ss = function(x){ 
  sum((x - mean(x, na.rm = T))^2, na.rm = T)
}

# Split data for crossvalidation
cv_folds = function(n, folds = 10){
  split(sample(1:n), rep(1:folds, length = n))
} 

# Computes the shortest distance between a point with coordinates (x,y) and the main diagonal
dist_point_line = function(x,y){
  return((x-y)/sqrt(2))
}

# Coefficient of variations
rob_cv = function(x) mad(x, na.rm = T)/median(x, na.rm = T)
cv = function(x) sd(x, na.rm = T)/mean(x, na.rm = T)

first <- function(x) { head(x, n = 1)[1] }
fin <- function(x) { tail(x, n = 1)[1] }
but.last <- function(x) { head(x, n = -1) }

# Shift elements of a vector n positions to the right (left: invert=T)
shift <- function(x, n, invert = FALSE, default = NA){
  stopifnot(length(x) >= n)
  if(n == 0){ return(x) }
  n <- if.else(invert, n*(-1), n)
  if(n < 0){
    n <- abs(n)
    forward = FALSE
  }else{
    forward = TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}

sum4 = function(v){
  m = mean(c(0, v[5:length(v)]))
  return(c(v[1]+m, v[2]+m, v[3]+m, v[4]+m))
}

# Reset par (graph) values
set_par_default <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
}

get_duplicated = function(v){
  v[duplicated(v)] %>% unique
}
