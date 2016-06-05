
## makeCacheMatrix reads and writes a matrix to a "cache". This isn't a true cache.
## All R functions are objects. The two variables matrixUnsolved and matrixSolved
## are object-level variables and as such they only persist for as long as the object
## (the makeCacheMatrix function in this case) is in memory and hasn't been 
## deallocated by the garbage collector.

## Parameters:
## x is an invertable numeric matrix. The function does not verify that the
## matrix is invertable.

makeCacheMatrix <- function(matrixUnsolved = matrix(numeric(0), 0, 0)) {

## Constructor code

## The unsolved matrix is initialized to the empty matrix or it's passed in
## by the user. By default, we don't set it to NULL because we want to check
## to make sure it's numeric later.

## Initialize the solved matrix to NULL so the user has an easy way to determine
## whether it has been set. 
        matrixSolved <- NULL

## The user might have passed in a matrix during instantiation. 
## Make sure it's numeric.
	if (!is.numeric(matrixUnsolved)) {
		message("matrix must be numeric")
		matrixUnsolved <- matrix(numeric(0), 0, 0)
		return
	}

## Public methods

## Sets the unsolved matrix to a new value. 
## If the new unsolved matrix is identical to the current unsolved matrix 
## then don't do anything. 
## If the new one is different then set it to the new value and null out 
## the solved matrix because it no longer matches the unsolved matrix.
        set <- function(newUnsolvedMatrix) {
		if (!is.numeric(newUnsolvedMatrix)) {
			message("matrix must be numeric")
			return
		}
		if (!identical(matrixUnsolved, newUnsolvedMatrix)) {
                	matrixUnsolved <<- newUnsolvedMatrix
                	matrixSolved <<- NULL
		}
        }

## Gets the unsolved matrix.
        get <- function() {
		matrixUnsolved
	}

## Sets the solved matrix. 
        setMatrix <- function(newSolvedMatrix) {
		if (!is.numeric(newSolvedMatrix)) {
			message("matrix must be numeric")
			return
		}
		matrixSolved <<- newSolvedMatrix
	}

## Gets the solved matrix.
        getMatrix <- function() {
		matrixSolved
	}

## Creates the list of functions.
        list(set = set, 
	     get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## cacheSolve calculates the inverse of a matrix.

## Parameters:
## cacheFunction is the name of the function that reads/writes the cache.
## ... are other named arguments which will be passed to solve().

cacheSolve <- function(cacheFunction, ...) {

## See if the inverted matrix exists in the cache. If it is, return it
        mSolved <- cacheFunction$getMatrix()
        if(!is.null(mSolved)) {
                message("getting cached data")
                return(mSolved)
        }

## The inverted matrix wasn't in the cache. Get the unsolved matrix
        data <- cacheFunction$get()

## Solve (i.e., invert) the matrix
        mSolved <- solve(data, ...)

## Put the inverted matrix into the cache
        cacheFunction$setMatrix(mSolved)

## Return the inverted matrix
        mSolved

}

