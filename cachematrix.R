## In general, this is a practice of lexical scooping in R. Function `makeCacheMatrix`
## creates an object that stores a matrix and cache its inverse.

## this function creates an object which contains 4 functions:
## 1. set value of a matrix 
## 2. get value of the matrix 
## 3. set value of the inverse matrix 
## 4. get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse matrix of a given matrix only when it is not cached
## otherwise, it returns cached value directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message('getting cached data')
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
