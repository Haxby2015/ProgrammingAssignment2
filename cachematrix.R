## makeCacheMatrix caches a matrix and its inverse, 
## cacheSolve allows cached inversion to be returned 
## or a new matrix to be stored, inverted and returned


## makeCacheMatrix - caches a matrix and its inverse

makeCacheMatrix <- function(basicM = matrix()) {
# basicM - initial matrix
        invertedM <- solve(basicM)
#setVals - sets the cached matrix and then inverts it
#	 - returns the inverted matrix
        setVals <- function(basic = matrix()) {
		basicM <<- basic
		invertedM <<- solve(basicM)
		invertedM
	}
#getInvert - returns the cached inverted matrix
        getInvert <- function() invertedM
#getMatrix - returns the cached matrix
	getMatrix <- function() basicM

	list(getInvert = getInvert, getMatrix = getMatrix, setVals = setVals)
}


## cacheSolve returns inverse of matrix. If a new matrix is supplied, then check if it is a genuine new matrix or the old
## one (either the exact old one or a new matrix with the same values)

cacheSolve <- function(caMa, newM = NULL, ...) {
# caMa - makeCacheMatrix
# newM - optional new matrix
# get current settings
        inv <- caMa$getInvert()
	bas <- caMa$getMatrix()
# if bas is null then makeCacheMatrix has not been set up yet
	if (is.null(bas)) {
		message("No matrix set");
		inv = NULL
	}
# if a new matrix has been supplied then compare it to the old one
# only interested in contents, not row or column names, which won't affect the
# value of the inversion
	 else if (!is.null(newM)) {
		if (!isTRUE(all.equal(bas, newM))) {
# okay then, return the new value after resetting the matrix and inverted matrix
			inv <- caMa$setVals(newM)
		}
	} 
# else we can use the returned inverted value
	else if (!is.null(inv)) {
                	message("Using cached value")
	}
# Single point of return from cacheSolve
        inv
}
