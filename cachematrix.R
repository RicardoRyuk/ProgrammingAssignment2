## Put comments here that give an overall description of what your
## functions do

## The two functions save runtime when an inverse matrix is used more than once in a program. 
## When an inverse matrix is calculated it is stored so that it can be retrieved and does not have to be calculated again. 
## More storage than necessary is used in order to save runtime.

## Write a short comment describing this function
## This function takes a matrix as an argument and turns it into a special matrix object. 
## It returns a list with four functions that permit the reading and writing of data. They allow the user to get and set the matrix as well as the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## The inverse matrix will be stored in i
        i <- NULL
        ## Reading and wirting - matrix
        set <- function(m){
                ## The <<- operator is used to keep the changes even after the function is finished.
                x <<- m
                i <<- NULL
        }
        get <- function() x
        ## Reading and writing - inverse matrix
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        ## Return
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes the special matrix object created by the makeCacheMatrix function. 
## If the inverse matrix has already been calculated the function will retrieve it from the cache.
## If it hasn't been calculated the function will calculate the inverse matrix and return it.

cacheSolve <- function(x, ...) {
        ## Check if the inverse has already been calculated
        i <- x$getinverse()
        if(!is.null(i)){
                message("Retrieving inverse matrix from cache")
                return(i)
        }
        ## Inverse not calculated yet
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
