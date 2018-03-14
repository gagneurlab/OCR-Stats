# Script with different useful functions

if.else = function(test, yes, no){
  if(test){ yes } else no
}

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


get_duplicated = function(v){
  v[duplicated(v)] %>% unique
}

## Useful seahorse functions
get_nhdf_from_cc <- function(cc, DT){
  pi <- get_plate_from_cc(cc, DT)
  nh_cc <- unique(DT[plate_id %in% pi & Fibroblast_id == "NHDF", cell_culture])
  if(length(nh_cc) == 0) return ("NA")
  return(nh_cc)
}

get_nhdf_from_fib = function(fib, DT){
  pi = get_plate_from_fib(fib, DT)
  unique(DT[plate_id %in% pi & Fibroblast_id == "NHDF", cell_culture])
}

get_plate_from_cc <- function(cc, DT){
  if(!cc %in% DT$cell_culture)
    stop("Sample does not exist.")
  pi <- unique(DT[cell_culture %in% cc, plate_id])
  return(pi)
}

get_plate_from_fib <- function(fib, DT){
  if(! fib %in% DT$Fibroblast_id)
    stop("Sample does not exist.")
  pi <- unique(DT[Fibroblast_id %in% fib, plate_id])
  return(pi)
}
