## R Programming Course on Coursera - Week 3 - Programming Assignment # 2
## ------------------------------------------------------------------------------------------------------------------
## There are two functions in this R code 
## The first function creates a list that has four functions for matrix operations - assigning and returning a matrix AND assigning and returning the inverse
## The second function computes the inverse of the matrix if it is not in cache already
## ---------------------------------------------------------------------------------------------------
## makeCacheMatrix is a function designed for matrix operations. It includes a list of four functions listed below
## 	1. setMatrix assigns the input matix (y) to the matrix x and sets the inverse of the matrix to NULL
##	2. getMatrix simply returns the matrix
##	3. setInverse stores value of the input for inverse in a variable inv 
##	4. getInverse simply returns the inverse of the matrix
## ------------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMatrix <- function(y) {
		x <<- y						# assign input matrix to the matrix x
		inv <<- NULL					# assign NULL value to the inverse
	}
	getMatrix <- function() x 				# return the matrix		
	setInverse <- function(inverse) inv <<- inverse		# store value of inverse
	getInverse <- function() inv				# return the inverse	

        list(setMatrix = setMatrix, getMatrix = getMatrix,	# return the list of functions for martix operations
             setInverse = setInverse,
             getInverse = getInverse)
}
## ------------------------------------------------------------------------------------------------------------------
## cacheSolve is a function designed to compute the inverse of a matrix
## The function first checks to see if the inverse already exists and is not NULL
## If the inverse already exists in memory, function returns a message with the value
## Otherwise, the function gets the matrix, computes its inverse, sets and returns the inverse 
## ------------------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()					# get the current value of inverse
	if (!is.null(inv)) {					# check to see if inverse is NULL
		message ("Returning cached inverse of the matrix")
		return(inv)					# return the cached value of the inverse
	}
	data <- x$getMatrix()					# if inverse is NULL, get the matrix
	inv <- solve(data)					# compute the inverse of the matrix
	x$setInverse(inv)					# set the value for the inverse
	inv							# return the inverse
}