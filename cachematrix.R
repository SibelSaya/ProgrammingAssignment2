#set overall function of list of 4 functions to run from matrix input
makeCacheMatrix <- function(x = matrix()) {
        #resets i in the local environment which will be the inverse matrix
        #(or has been already cached as as value)
        i <- NULL
        #func1 that sets the value of the matrix
        set <- function(y) {
                #sets y in local to x in parent
                x <<- y
                i <<- NULL
        }
        #func2 that sets get to x (i.e. the matrix data)
        get <- function() x
        #func3 that sets setinv to a function of i
        setinv <- function(inv) i <<- inv
        #func4 that sets getinv to i (i is the inverse matrix)
        getinv <- function() i
        #makes list of these 4 functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#new function where makeCacheMatrix will be an argument, checks for cache of
#inverse matrix and if not there then makes an inverse of the matrix x
cacheSolve <- function(x, ...) {
        #sets i to the value of the inverse matrix already cached. If there's
        #nothing to be set then it is NULL
        i <- x$getinv()
        #checks if i is NULL or not
        if(!is.null(i)) {
                #if it is not null (i.e. there is already an inverse matrix
                #specified then prints message and returns the cached matrix)
                message("getting cached data")
                return(i)
        }
        #else if puts the matrix into data
        data <- x$get()
        #sets i to the inverse of the matrix
        i <- solve(data, ...)
        #sets setinv to the inverse
        x$setinv(i)
        #returns the inverse matrix
        i
}
