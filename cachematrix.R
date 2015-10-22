## R Programming Course on Coursera - Week 3 - Programming Assignment # 2
## ----------------------------------------------------------------------------------------------
## There are two functions in this R code 
## The first function creates a list that has four functions for matrix operations - assigning and returning a matrix AND assigning and returning the inverse
## The second function computes the inverse of the matrix if it is not in cache already
## ----------------------------------------------------------------------------------------------
## makeCacheMatrix is a function designed for matrix operations. It includes a list of four functions listed below
## 	1. setMatrix assigns the input matix (y) to the matrix x and sets the inverse of the mean to NULL
##	2. getMatrix simply returns the matrix
##	3. setInverse stores value of the input for inverse in a variable inv 
##	4. getInverse simply returns the inverse of the matrix
## ----------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMatrix <- function(y) {
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function() x 			
	setInverse <- function(inverse) inv <<- inverse # this function 
	getInverse <- function() inv			# this function 

        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}
## ----------------------------------------------------------------------------------------------
## cacheSolve is a function designed to compute the inverse of a matrix
## The function first checks to see if the inverse already exists and is not NULL
## If the inverse already exists in memory, function returns a message with the value
## Otherwise, the function gets the matrix, computes its inverse, sets and returns the inverse 
## ----------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message ("Returning cached inverse of the matrix")
		return(inv)
	}
	data <- x$getMatrix()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}