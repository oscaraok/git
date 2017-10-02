# Programming Assingment 2 in Week 3. 
# Lexical Scoping 
# makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
  
    #define the actions of the function
    # return the existing info
    get = function() x
    # set the inverse cache and inv to al environments
    setinv = function(inverse) inv <<- inverse 
    # return the data in cache
    getinv = function() inv
    # return actions in object
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# Programming Assingment 2 in Week 3. 
# Lexical Scoping 
# CacheSolve()

cacheSolve <- function(x, ...) {
    inv = x$getinv()
  
    # if the inverse does exist
    if (!is.null(inv)){
        # get it from the cache and return it. 
        message("getting cached data")
        return(inv)
    }
  
    # if needed then calculate the inverse 
    datObj = x$get()
    # invert and pass other args as needed
    inv = solve(datObj, ...)
    # set the value of the cache
    x$setinv(inv)
    # return the inversed object
    return(inv)
}
