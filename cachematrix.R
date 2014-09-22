## Two functions to cache the inverse of a matrix
## Creates a matrix object that  cache their inverse
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

    ## Funtion to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Function for  getting the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Function to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Function to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse
        i
    }

    ## Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate the inverse of the original matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" will get the inverse from cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getInverse()

    ## Return inverse if its  set
    if( !is.null(inv_x) ) {
            message("getting cached data")
            return(inv_x)
    }

    ## Get the matrix from our object
    new_matrix <- x$get()

    ## Calculate the inverse using matrix multiplication
    inv_x <- solve(new_matrix) %*% new_matrix

    ## Set the inverse to the object
    x$setInverse(inv_x)

    ## Return the matrix
    inv_x
}