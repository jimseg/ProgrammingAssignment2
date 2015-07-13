## create a list of functions to store a matrix and compute its inverse
## only once. Computing the inverse is deferred until the inverse is
## requested and the inverse is stored to avoid the overhead of re-compyting
## it if the matrix is unchanged

## return a list of functions associated with a matrix:
##      can be called with an initial matrix (acts as if the set function
##          was called)
##      if called with report = TRUE, prints a message whenever solve() is
##         called because there is no cached inverse
##      set(m) - store a matrix, mark inverse as not cached
##      get()  - return a copy of the stored matrix
##      setinverse(m) - store m as the cached value of the inverse
##      getinverse()  - return the inverse of the stored matrix,
##                      when available, return the cached value

makeCacheMatrix <- function(x = matrix(), report = FALSE) {
    ## save initial matrix, mark inverse not computed
    inv <- NULL
    mat <- x
    set <- function(m = matrix()) {
        ## store new matrix, inverse is not computed
        mat <<- m
        inv <<- NULL
    }
    get <- function() {
        ## juat return current matrix
        mat
    }
    setinverse <-  function(m = NULL) {
        ## store an inverse matrix to avoid using solve - useful if
        ## the caller has some preferred method of getting the inverse
        ## risks setting an inverse which is not related to the stored
        ## matrix
        inv <<- m
    }
    getinverse <- function() {
        ## get the inverse, using saved value if its been computed
        if(is.null(inv)) {
            if(report) {
                message("No cached inverse, computing it now")
            }
            inv <<- solve(mat)
        } 
        inv
    }
    ## return the list of functions
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)

}

## return the inverse of a matrix refered to by the list
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    x$getinverse()
}
