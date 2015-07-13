## load cachematrix.R and exercise it with four tests
## uses a 3x3 numeric matrix and a complex matrix taken from the help pages

test_cachesolve <- function() {
    source("cachematrix.R")
                                        #
    ## 3x3 matrix which has an inverse        
    m <- matrix(byrow = TRUE, nrow = 3, c(2, 3, 4, 1, 2, 6, 4, 5, 2))
    ## manually computed inverse of m
    i <- matrix(byrow = TRUE, nrow = 3, c(-13, 7, 5, 11, -6, -4, -1.5, 1, 0.5))

    x = makeCacheMatrix(m, report=TRUE)
    message("Does get function return original matrix?")
    print(identical(x$get(), m))
    message("converting inverse of m, expect report")
    invm <- cacheSolve(x)
    ## verify that we got the expected inverse - no errors more than
    ## 1e-14
    if(sum(abs(as.vector(i - invm)) < 1e-14) == 9) {
        message("Inverse of m has expected values")
    } else {
        message("inverse of m appears to be wrong")
        message("Got"); print(invm); message("Expected:");print(i)
    }
    message("Getting inverse of m again, should not report recomputing")
    invm1 <- cacheSolve(x)
    message("Is cached inverse unchanged?")
    print(identical(invm, invm1))
    message("Does set function install a new matrix?")
    m2 <- matrix(byrow = TRUE, nrow = 3, c(1, 3, 3, 1, 4, 3, 1, 3, 4))
    i2 <- matrix(byrow = TRUE, nrow = 3, c(7, -3, -3, -1, 1, 0, -1, 0, 1))
    x$set(m2)
    print(identical(m2, x$get()))
    message("Getting inverse of new matrix, expect report")
    invm2 <- cacheSolve(x)
    message("Does cacheSolve deliver same inverse (no report expected)?")
    print(identical(invm2, cacheSolve(x)))
    if(sum(abs(as.vector(i2 - invm2)) < 1e-14) == 9) {
        message("Inverse has expected values")
    } else {
        message("inverse appears to be wrong")
        message("Got"); print(invm2); message("Expected:");print(i2)
    }
    message("Tests completed")
}


    

                
