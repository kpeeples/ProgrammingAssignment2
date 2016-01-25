## Assignment: Caching the Inverse of a Matrix
## 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## In this Programming Assignment will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.
## For this assignment, assume that the matrix supplied is always invertible.

## Function - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## 

makeCacheMatrix <- function(X = matrix())
{
	inverse <- NULL
	set <- function(y)
	{
		X <<- y
		inverse <<- NULL
	}
	get <- function() X
	setinverse <- function(vInverse) inverse <<- vInverse
	getinverse <- function() inverse
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## assumes a package like corpcor has been installed and loaded from a CRAN mirror
## type ??inverse or for an example which contains pseudoinverse: http://127.0.0.1:15599/library/corpcor/html/pseudoinverse.html
## Function - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) 
{
	inverse <- X$getinverse()
	if (!is.null(inverse))
	{
		message("Cached Data")
		return(inverse)
	}
	message("Not Cached Data")
	data <- X$get()
	inverse <- pseudoinverse(data, ...)
	X$setinverse(inverse)
	inverse
}

