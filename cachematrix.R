## makeCacheMatrix stores cached values of matrix and inverted matrix
## cacheSolve works out if matrix has changed and calls makeCacheMatrix to reset matrices (basicM and invertedM) if it has

## makeCacheMatrix - stores a matrix and its inverted value
#       basicM - the cached matrix
#       invertedM - the inversion of basicM (created using 'solve()')

makeCacheMatrix <- function(basicM = matrix()) {
#       basicM - initial matrix
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


## cacheSolve - returns inverted matrix, either cached value or new one if newM is not null

cacheSolve <- function(caMa, newM = NULL, ...) {
#       caMA - makeCacheMatrix function
#       newM - optional new matrix

        # get current cached settings
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
