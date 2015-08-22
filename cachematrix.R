## Put comments here that give an overall description of what your
## functions do

#Overall, makeCacheMatrix defines four functions that are then placed into
#a list. One of these stores in cache the value of solve(x). This list is
#then accessed in cacheSolve. If the matrix inverse is not equal to stored
#cache value, the inverse is then calculated. Citation: I used the example problem
#and also looked at programming assignment two forums in Coursera R class.

## Write a short comment describing this function

#In this function you first set makeCacheMatrix to a function that takes as
#input an empty matrix. Within this parent environment you have four functions.  
#Set allows you to input a variable and set makeCacheMatrix input to it and stores m as NULL. 
#Get searches the parent environment for the input x. 
#Setinv stores as m solve input put in makeCacheMatrix.
#Get inv then returns m.
#A list is created where each function is set to a different name
#in the list.

makeCacheMatrix <- function(x = matrix()){
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setinv<-function(solve) m<<-solve
	getinv<-function()m
	list(set=set,get=get,
	     setinv=setinv,
	     getinv=getinv)
}


## Write a short comment describing this function
#This function takes as input an object set to the output of makeCacheMatrix(x)
#It first extracts the inverse of x at the name getinv. If it is not NULL,
#then it returns the stored value. Otherwise, if it is NULL, then
#it extracts the matrix stored at name get, calculates the inverse and stores it as m
#then sets the inverse to the new inverse matrix stored as m.

cacheSolve <- function(x, ...) {
	m<-x$getinv()#extracts the inverse of x at the name setinv()
	if(!is.null(m)){
		message("getting cached matrix")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinv(m)
	m
}

