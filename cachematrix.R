##----This function takes in the value of the matrix
        
makeCacheMatrix<-function(x){
  I<-NULL ##----The inverse is first set to NULL just in case
    set<-function(y){
    x<<-y ##----The value of matrix is cached to a variable 
    I<<-NULL ##----value of inverse to NULL
  }
        get<-function()x      ##----the value of matrix is obtained
        setInv<-function(Invs)I<<-Invs    ##----The value of the inverse of the matrix is cached as returned by the computing function "cacheSolve"
        getInv<-function()I     ##----The value of the inverse cached is returned to the calling function
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
        
cacheSolve<-function(y){
  I<-y$getInv() ##----The cached value of the inverse is obtained and displayed if available
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  x<-y$get() ##----If Inverse value returned is NULL, then value of matrix is once again obtained freshly
  I<-solve(x) ##----The inverse of above obtained matrix is computed
  y$setInv(I) ##----The inverse of matrix computed is cached in the inverse variable for possible future use.
  I ##----The computed inverse of matrix is displayed
}
