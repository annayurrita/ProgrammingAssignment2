## Function 1 creates a vector to cache the inverse of a matrix. 
## First, identify the 2 objects: x and s. 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      ## Then define the set function. 
      set <- function(y){
        x <<- y
        s <<- NULL
      }
      ## Remember to set s to NULL again. 
      ## Then create the getter function for the vector x. 
      get <- function() x
      ## Define the setter for the solve function. 
      setSolve <- function(solve) s <<- solve
      ## Define the getter for the solve function. 
      getSolve <- function() s
      ## Assign each of the elements within a list and returns it to the parent
      ## environment. 
      list(set = set, # gives the set name to the set() function. 
           get = get, # gives the get name to the get() function. 
           setSolve = setSolve, # gives the name setSolve to its function. 
           getSolve = getSolve) # and the name getSolve to its function. 
}


## Function 2 calculates the inverse of a matrix and stores in the cache above. 

cacheSolve <- function(x, ...) {
      ## First the function attempts to retrieve a mean from the object 
      ## passed in as the argument. 
      s <- x$getSolve()
      ## Then it checks if the object is set to null, otherwise it returns what 
      ## is stored. 
      if(!is.null(s)){
          message("getting cache data")
          return(s)
      }
      ## Now it gets the the vector from the input object. 
      data <- x$get()
      ## Calculates the solve function. 
      s <- solve(data, ...)
      ## Uses the function on the input object to set the result. 
      x$setSolve(s)
      ## Returns the result to the parent environment. 
      s
}
