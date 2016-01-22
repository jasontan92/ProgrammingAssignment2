## The combination of these two functions allows the storing, alteration and retrieval of 
## input data as well as output data(inverse values)

## makeCacheMatrix is a list of four functions, set, get, setinverse and getinverse.
## Set allows the overwrite of the cached data (x) cached in the function, while get
## retrieves the data that was most recently being stored in set

## Similarly, setinverse allows the caching or overwrite of previously cached output data (inverse i)
## and getinverse retrieves the most recently calculated/cached inverse values

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){
    return(x)
  }
  setinverse<-function(inverse){
    i<<-inverse
  }
  getinverse<-function(){
    return(i)
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve first checks if there is a previously cached inverse value, and returns it.
## If there is no currently present inverse value,  cacheSolve then uses the function solve to
## calculate a new value of inverse, and then cache it within the set function and 
## return it at the same time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  return(i)
}
