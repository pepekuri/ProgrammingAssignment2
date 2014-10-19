################
# Assignment 2 #
################
#
## Note: Both function are based on the assignment example.
#
#####################################################################
#  1. Write a short comment describing the makeCacheMatrix function #
#####################################################################
#
#   Function name: makeCacheMatrix
#
#   Function Description: It defines the structure for storing the input matrix and its inverse matrix and sotre it on cache.
#
#   Variables:
#       x (input variable): stores the original matrix before calculate the inverse matrix over it.
#       m: this variable is for store the inverse matrix.
#
#   Functions:
#       pSet: sets the input matrix in "x", and also resets the function variable m.
#       pGet: returns the value of the variable "m"; the "m" variable stores the inverse of the input matrix.
#       psetinvmtx: sets the value of the variable "m" (set the value of the inverse matrix variable).
#       pgetinvmtx: gets the value of the variable "m" (gets the value of the inverse matrix variable).
#
#   Returned value (List):
#       This function returns a list containing the references to the four functions mentioned before.

makeCacheMatrix <- function(x = matrix()) {
    #m stores the inverse matrix variable
    m <- NULL
    
    pset <- function(y) {
        m <<- NULL
        x <<- y
    }
    
    pget <- function() {
        return(x)
    }
    
    psetinvmtx <- function(z) {
        m <<- z
        return(m)
    }
    
    pgetinvmtx <- function() {
        return(m)
    }
    
    return(list(set = pset, get = pget, setinvmtx =  psetinvmtx, getinvmtx = pgetinvmtx))
}

##############################################################
# 2. Write a short comment describing the cacheSolve function#
##############################################################
#
#   Function name: cacheSolve
#
#   Function Descripition:  Returns the inverse matrix of "x". 
#                           The function validates if the input function satisfy the inverse matrix computing requirements (x must be square and Dx <> 0).
#                           The inverse matrix is computed only if it was not computed before.
#                           If the inverse matrix was computed before, then it only returns the computation value avoiding to compute it again.

cacheSolve <- function(x, ...) {
    
    #x must be square and Dx <> 0
    if (nrow(x$get()) != ncol(x$get())){
        stop("Impossible to compute inverse matrix: Input Matrix must be a square one")
    }
    else if (det(x$get()) == 0){
        stop("Impossible to compute inverse matrix: Input Matrix Determinant = 0")
    }
    else{
        #Only if it is possible to compute the inverse matrix
        if(!is.null(x$getinvmtx())) {
            print("Getting inverse matrix")
            return(x$getinvmtx())
        }
        else {
            print("Calculating inverse matrix")
            m <- solve(x$get(), ...)
            x$setinvmtx(m)
            return(x$getinvmtx())
        }
    }
}

#############################
# 3.Sample code for testing #
#############################

x <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
m1 <- makeCacheMatrix(x)

#First execution
cacheSolve(m1)

#Second execution
cacheSolve(m1)