makeCacheMatrix<-function(x){
  I<-NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function()x
  setInv<-function(Invs)I<<-Invs
  getInv<-function()I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}

cacheSolve<-function(y){
  I<-y$getInv()
  if(!is.null(I)){
    return(I)
  }
  x<-y$get()
  I<-solve(x)
  y$getInv()
  I
}
