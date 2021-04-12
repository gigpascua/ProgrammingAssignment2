## this text file is made for Programming Assignment 2 
## This will show how Lexical Scoping works 
## First, it will create a matrix that has get & set functions
## it will just print the cache from repeatedly printing the inverse matrix 
## it will prevent the program from looping 

makeCacheMatrix <- function(mat = matrix()) {
  
  # will print NULL if mat is not a matrix
  inv <- NULL
  
  #set for matrix
  set <- function(rix) {
    mat <<- rix
    inv <<- NULL
  }
  
  #get for matrix, set for inv, then get for inv
  get <- function() mat
  set_inverse <- function(set_inv) inv <<- set_inv
  get_inverse <- function() inv
  
  #the program prints the list of set & get functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve will proceed on solving the inverse of matrix from input
## Ifelse function is used
## If already calculated, else print cache

cacheSolve <- function(cd_matrix, ...) {
  inv <- cd_matrix$get_inverse()
  if(!is.null(inv)) {
    message("printing inverse of cache matrix")
    return(inv)
  }
  rfm_data <- cd_matrix$get()
  inv <- solve(rfm_data, ...)
  cd_matrix$set_inverse(inv)
  inv
}
