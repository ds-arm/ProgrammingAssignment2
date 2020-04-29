## This function returns a list containing the matrix used to initialise it, 
## plus a cached version of its solution (once it is calculated using 
## cacheSolve)
## Note - when solve() (and cacheSolve()) are passed one matrix argument it 
##      returns the inverse.  The assignment was directed to this use case.
##      See help(solve) for more detail.

makeCacheMatrix <- function(x = matrix()) {
    sol_x <- NULL     #  When initialised, set value of solution to NULL
    
    setmatrix <- function(updatedMatrix){
        x <<- updatedMatrix     # Update x in the makeCacheMatrix env 
                                #     to the new matrix
        sol_x <<- NULL          # New matrix so reset solution to NULL
    }
    
    getmatrix <- function(){
        x               # This function returns value of the current matrix 
    }
    
    setsolve <- function(calculatedsolve){
        sol_x <<- calculatedsolve
    }
    
    getsolve <- function(){
        sol_x          # Return the value from the makeCacheMatrix env
    }
    
    ## Return a list of named elements where each is a function
    list(set = setmatrix, get = getmatrix, 
         setsol = setsolve,
         getsol = getsolve)

}


## This function is used to return the solve of the matrix described by 
## the makeCacheMatrix list.  If it has already been calculated and cached, 
## this function returns the cached value. If it does not have a cached solution 
## this function calculates it (using the R function solve()) and then caches 
## this value in the makeCacheMatrix list before returning the calculated value.
## 
## Note - following has been limited to using solve() to find the inverse of 
##      'x', and it takes no additional arguments.  See help(solve) for more.  

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x' using solve() with one arg
    ## Note:  'x' needs to be a makeCacheMatrix list object
    
    x_sol <- x$getsol()    # Get the value currently held in the cache
    if(!is.null(x_sol)){   # If not NULL then it has been already calculated
        message('using a cached version')
        return(x_sol)
    }
    
    # Continues if cache is NULL
    # Calculate the solution (= inverse) for matrix 'x', cache it, return it
    currMatrix <- x$get()
    
    x_sol <- solve(currMatrix)    # Use solve to calc solution = inverse
    
    x$setsol(x_sol)     # Use the setsol function to cache 
                        # the calculated solution
    
    x_sol               # Return the solution
}
