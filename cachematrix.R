## The following functions calculate the inverse of a matrix and saves it
## This function creates a special "matrix" object 
## Stage1:- Initially set the value of the matrix
## Stage2:- get the value of the matrix
## Stage3:- set the value of the inverse
## Stage4:- get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## inverse of the "matrix" created using following function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
##test cases 
# > a <- matrix(rnorm(4),2,2)
# > b <- makeCacheMatrix(a)
# > cacheSolve(b)

#            [,1]      [,2]
# [1,] -0.8743648 0.1437579
# [2,] -1.4401466 2.1883039

# > a <- matrix(rnorm(9),3,3)
# > b <- makeCacheMatrix(a)
# > cacheSolve(b)

#             [,1]       [,2]        [,3]
# [1,] -0.03061152 0.31859366  0.97103426
# [2,] -0.42497909 0.09200919 -0.01964536
# [3,]  0.19672716 1.53248447  0.89787772

# > a <- matrix(rnorm(16),4,4)
# > b <- makeCacheMatrix(a)
# >  cacheSolve(b)

#            [,1]      [,2]       [,3]      [,4]
# [1,] -0.1764380  2.820554  1.0299139  1.345434
# [2,]  0.1791698 -3.276911 -0.7646985 -1.895740
# [3,] -0.7086666  2.963787  0.6976065  1.400591
# [4,] -0.3764773  2.775705  1.1543025  2.168745

# > a <- matrix(rnorm(25),5,5)
# > b <- makeCacheMatrix(a)
# > cacheSolve(b)

#            [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  0.9728966 -0.23124359 -0.1099162 -0.2801211  0.6892089
# [2,] -0.5112995  0.62507295 -0.7878250 -0.1605402  0.2527291
# [3,]  0.2549992  0.04080565  0.7148973  0.3321964 -0.1181749
# [4,] -0.1957071 -0.19219140 -1.1330432  0.3220317 -0.8204162
# [5,] -0.8672421  0.43364821 -0.9097335 -0.3243333 -0.8379884
