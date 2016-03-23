## Assignment 3:Prashant Duhoon

## Matrix object to create cache

makeCacheMatrix <- function(x = matrix()) {
	xi<-NULL
	setmat<-function(set) {
                  x<<- set
                  xi<<-NULL}
	getmat<-function()x
	setinv <- function(inv=matrix()) xi <<- inv
      getinv <- function() xi
      list(set=setmat,get=getmat,setinverse = setinv,getinverse = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      xi<-x$getinverse()
	if(!is.null(xi)){
		message("using cached matrix")
            return(xi)}
	mat<-x$get()
	xi<-solve(mat,...)
	x$setinverse(xi)
	xi
}
