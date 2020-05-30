## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a matrix by taking the matrix as argument that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()
        x
    setminverse <- function(inverse)
        i <<- inverse
    getminverse <-function()
        i
    list(set = set, get = get,
         setminverse = setminverse,
         getminverse = getminverse)
}


## Write a short comment describing this function
## Calculate inverse of a matrix from makeCacheMatrix function & if the inverse is already comuted 
## then return it from cache otherwise this cacheSolve function compute it & save it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getminverse()
    if(!is.null(m)){
        message("getting inverse from cache")
        return(m)
    }
    b <- x$get()
    c <- solve(b)
    x$setminverse(c)
    c
}
