## Put comments here that give an overall description of what your
## functions do
# need to create a list containing functions to set the matrix (x) & inverse (i),
# get the matrix from the parent environment, 
# set the inverse (setinverse), get the inverse (getinverse)


makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # this sets x and i in the parent environment, setting i to NULL
    get <- function() x
    # this gets x from the parent environment
    setinverse <- function(inverse) i <<- inverse
    #sets i in parent environment
    getinverse <- function() i
    #gets i from parent environment
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    #creates a named list
}


## Checks if i has already been set & returns it. If not, sets thematrix to the 
# one in the parent, inverts it and sets i again

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting data from cache")
        return(i)
    }
    thematrix <- x$get()
    i <- solve(thematrix)
    x$setinverse(i)
    i
}

## to test
# testmatrix <- matrix(1:4, byrow=TRUE, nrow=2)
# solve(testmatrix) would normally return
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# pass testmatrix to makeCacheMatrix
# make <- makeCacheMatrix(testmatrix)
# then pass the result to cacheSolve
# cacheSolve(make)
# first pass should return the matrix
# second pass should return 'getting data from cache' and the matrix