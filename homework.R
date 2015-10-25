## set the value of the vector / get the value of the vector / set the value of the mean / get the value of the mean ##
makeVector <- function(x = numeric()) # x is a numeric vector that is the argument for the makeVector function 
{
    m <- NULL # m is a new variable being created inside this function with a NULL value assigned to it
    set <- function(y) # set is a new function being created with new variable y as its input 
    {
        x <<- y # inside this new function, the input variable y is being assigned as the value of x, which exists in a different environment: the function that contains this function
        m <<- NULL # inside this new function, a NULL value is being assigned to the variable m which exists in a different environment: the function that contains this function (this is necessary because function set changes the data in y by making the value of x, so the old mean is no longer valid)
    }
    get <- function(){
        return(x)   ## get is a new function that returns x (the numeric vector that is the input for makeVector)
    }
    setblarg <- function(blarg) {
        m <<- blarg ## setblarg is a new function that returns m, which has been defined in the next higher environment (the makeVector function) and is now being assigned the value of "blarg"
    }
    getblarg <- function() m ## getblarg is a new function that returns the value of m
    list(set = set, get = get,
         setblarg = setblarg,
         getblarg = getblarg)
    # this last step creates a list that will be output by the makeVector function; the components of the list are: set, which is the function created above; get, which is defined in line 10; setmean, which is defined in line 11; and getmean, which is defined in line 12.
}

## The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function. ##
cachemean <- function(x, ...) {
    m <- x$getblarg() # in this function, m is a new variable being created and assigned the value of the getblarg element in x, which is an input to the function being defined
    if(!is.null(m)) # the condition is "if m is not NULL"
    {
        message("getting cached data") # I want to see this text if the "if" condition is met
        return(m) # I want to spit out the value of m if the "if" condition is met
    }
    # none of these next lines happen if m above is not null; the following lines are a way to assign a value to m and spit it out to be save for later (basically, the next lines are an "else" statement)
    data <- x$get() # create a variable called data that's assigned the value of the get element in x (which is our numeric vector)
    m <- mean(data, ...) # apply the mean function to data and assign that result to m
    x$setblarg(m) # m is the argument we're inputting to the setblarg function we got from x
    m # spit out m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) # the argument(input) to this function is x, which is a matrix
    # can i calculate the inverse of matrix x?
    # if so, spit out the inverse of matrix x and save it for later
{
    m <- NULL # m is a new variable being created inside this function with a NULL value assigned to it
    set <- function(y) # set is a new function being created with new variable y as its input 
    {
        x <<- y # inside this new function, the input variable y is being assigned as the value of matrix x, which exists in a different environment: the function that contains this function
        m <<- NULL # inside this new function, a NULL value is being assigned to the variable m which exists in a different environment: the function that contains this function (this is necessary because function set changes the data in y by making the value of x, so the old matrix is no longer valid)
    }
    get <- function(){
        return(x)   ## get is a new function that returns x (the matrix makeCacheSolve)
    }
    setblarg <- function(blarg) {
        m <<- blarg ## setblarg is a new function that takes argument "blarg" and returns m, which has been defined in the next higher environment (the makeCacheSolve function) and is now being assigned the value of "blarg."
    }
    getblarg <- function() {
        return(m) 
        } ## getblarg is a new function that returns the value of m
    list(set = set, get = get,
         setblarg = setblarg,
         getblarg = getblarg)
    # this last step creates a list that will be output by the makeCacheSolve function; the components of the list are: set, which is the function created above; get, which is defined in line 10; setmean, which is defined in line 11; and getmean, which is defined in line 12.
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) # the argument(input) to this function is x and other unspecified arguments
{
    # look to see if the inverse of the matrix has already been calculated
    # if the inverse of the matrix has already been calculated, find it, then spit out that value and exit this function
    # if the inverse of the matrix has not been calculated, then we need to calculate it and spit out that value
    m <- x$getblarg() # in this function, m is a new variable being created and assigned the value of the getblarg element in x, which is an input to the function being defined
    if(!is.null(m)) # the condition is "if m is not NULL"
    {
        message("getting cached data") # I want to see this text if the "if" condition is met
        return(m) # I want to spit out the value of m if the "if" condition is met
    }
    # none of these next lines happen if m above is not null; the following lines are a way to assign a value to m and spit it out to be save for later (basically, the next lines are an "else" statement)
    data <- x$get() # create a variable called data that's assigned the value of the get element in x
    m <- solve(data, ...) # apply the solve function to data, producing the inverse of the matrix, and assign that result to m
    x$setblarg(m) # m, defined in the line above, is the argument we're inputting to the setblarg function we got from x
    m # spit out m, the inverted matrix
}
