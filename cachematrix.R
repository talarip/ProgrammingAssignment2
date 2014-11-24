
# makeVector creates a special "vector", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }


# Function makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of matrix inverse
# get the value of matrix inverse
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)
}


# This cacheSolve assumes that the matrix is always invertible. 
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# set_inverse function.

cacheSolve <- function(a, ...) {
  inv <- a$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$set_inverse(inv)
  inv
}


# # Testing
# #Create Cache matrix
# a = rbind(c(1, -2), c(-2, 1))
# m = makeCacheMatrix(a)
# m$get()
# 
# #No cache
# cacheSolve(m)
# #Cached
# cacheSolve(m)