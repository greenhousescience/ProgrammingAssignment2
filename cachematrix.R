## This function, "makeVector", creates a special "vector", which
## is really a list containing a function to 
## 1. set the value of the vector
##2. get the value of the vector
## 3. set the value of the inverse
##4. get the value of the inverse

makeVector <- function(x = numeric()) {
Inv <- NULL
set <- function(NewValue)  {
        x <<- NewValue
        Inv <<- NULL
}
get <- function() x
setInverse <- function() Inv <<- solve(x)
getInverse <- function() Inv
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This second function calculates the inverse of the special "vector"
## created with the above function. However, it first verifies if the
## inverse has already been calculated. If so, it "gets" the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the vector and sets its value in the cache via the
## "setInverse" function

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        If (!is.null(Inv))   
        message("Getting cached data")
        return(Inv)
               
data <- x$get()
        Inv <- solve(data)
        x$setInverse(Inv)
        Inv        
}
