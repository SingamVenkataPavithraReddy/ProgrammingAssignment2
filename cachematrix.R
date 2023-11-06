## Create a list container with caching capabilities for a numeric vector.
makeVector <- function(x = numeric()) {
        m <- NULL
        ## Define the 'set' function to set the vector's value.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Define the 'get' function to retrieve the vector's value.
        get <- function() x
        ## Define the 'setmean' function to set the cached mean value.
        setmean <- function(mean) m <<- mean
        ## Define the 'getmean' function to retrieve the cached mean value.
        getmean <- function() m
        ## Return a list of the functions for external use.
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Calculate the mean of a numeric vector with caching.
cachemean <- function(x, ...) {
        ## Retrieve the cached mean value if it exists.
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Calculate the mean if it's not cached.
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)  ## Cache the calculated mean.
        m  ## Return the calculated mean.
}
