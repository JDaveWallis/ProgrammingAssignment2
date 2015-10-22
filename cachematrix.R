## Submitted by Dave Wallis - 10/22/2015
## as part of the Coursera R Programming course, programming assignment 2.
##
##  This assignment demonstrates how to utilize cached memory variables that exist
##  outside of a function's environment in order to improve efficiency (by not having
##  to repeat the same expensive calculation for subsequent function calls).

##  The makeCacheMatrix function takes a matrix matx as an argument and returns
##  a list of functions used for the following actions:
##  - to set the matrix value in cached memory (using variable matx), 
##  - to get the matrix value (from variable matx), 
##  - to set the inverse of the matrix in cached memory (using variable matinv), 
##  - or to get the inverse of the function from cached memory (from variable matinv).
##
makeCacheMatrix <- function(matx = matrix()) {
  matinv <- NULL                      #initialize matinv local variable
  set <- function(y) {                #set() function assigns matrix value
    matx <<- y                  #from input argument y and caches it
    matinv <<- NULL             #in matx variable, also initializes
  }                                   #the cached matinv variable to null
  
  get <- function() matx              #get() function gets the cached matrix
  #value from matx
  setinverse <- function(solve) matinv <<- solve
  #setinverse() function calculates the
  #inverse of the matrix using the solve()
  #function and sets that in the cached
  #memory variable matinv.
  getinverse <- function() matinv     #getinv() gets the cached matrix inverse
  #value from matinv
  list(set = set, get = get,          #returning a list of functions allows
       setinverse = setinverse,       #these functions to be callable from
       getinverse = getinverse)       #outside the makeCacheMatrix function.
}


##  The cacheSolve function takes a matrix "matx" as an argument.  It then evaluates
##  whether the inverse of that matrix has already been determined via the Solve()
##  function and placed into the "matinv" variable, which is defined in the calling
##  environment.  If matinv has already been calculated, it returns that variable,
##  along with a message indicating the cached inverse is being returned.
##  But if matinv is null, it calculates the inverse of the matx matrix argument and 
##  sets that value in memory.
##
cacheSolve <- function(matx, ...) {
  matinv <- matx$getinverse()        #Determine if the matrix inverse
  #is already cached in memory
  if(!is.null(matinv)) {             #If the inverse is already cached,
    #return that cached value.
    message("getting cached matrix inverse")
    return(matinv)
  }                                  #This logic path is followed
  #when the matrix inverse is not cached.
  matrixdata <- matx$get()           #Use the get() function to get the matrix
  matinv <- solve(matrixdata, ...)   #solve() returns the matrix inverse.
  matx$setinverse(matinv)            #Cache the solved matrix inverse in memory.
  matx
}
