## set the value of the vector / get the value of the vector / set the value of the mean / get the value of the mean ##
makeVector <- function(x = numeric()) # x is a numeric vector that is the argument for the makeVector function 
{
    m <- NULL # m is a new variable being created inside this function with a NULL value assigned to it
    set <- function(y) # set is a new function being created with new variable y as its input 
    {
        x <<- y # inside this new function, the input variable y is being assigned as the value of x (the numeric vector), which exists in a different environment: the function that contains this function
        m <<- NULL # inside this new function, a NULL value is being assigned to the variable m which exists in a different environment: the function that contains this function (this is necessary because function "set" changes the data by making y the value of x, so the old mean is no longer valid)
    }
    get <- function() {
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

blu <- makeVector(c(1, 2, 5, 7, 8, 11, 15, 18, 23))
v <- blu$get()
s <- blu$getblarg()
fake.m <- blu$setblarg(NULL)
new.s <- blu$getblarg()

u <- cachemean(blu)
mean(v)

# test
B <- c(1, 5, 7)
C <- c(4, 2, 6)
D <- c(13, 17, 23)
mat1 <- cbind(B, C, D)
blu <- makeCacheMatrix(mat1)
cacheSolve(blu)
solve(mat1)
#