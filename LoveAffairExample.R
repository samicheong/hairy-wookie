## R example for modeling love affair as a discrete dynamical system (Math 675 Lab) 

# Initialize data arrays. There are 6 scenarios of dynamics between the two lovers
M.array = array(0,c(2,2,6))
#V.array = array(0,c(2,2,6))
#D.array = array(0,c(2,2,6))

# Assign values to each of the transition matrices
M.array[,,1]=matrix(c(0.5,0.2,0.5,0.7),2,2,byrow=TRUE)
M.array[,,2]=matrix(c(0.5,0.9,0.7,0.7),2,2,byrow=TRUE)
M.array[,,3]=matrix(c(0.5,0.2,0.5,0.8),2,2,byrow=TRUE)
M.array[,,4]=matrix(c(1,0.2,-0.2,1),2,2,byrow=TRUE)
M.array[,,5]=matrix(c(0.5,-sqrt(3)/2,sqrt(3)/2,0.7),2,2,byrow=TRUE)
M.array[,,6]=matrix(c(0.5,0.6,0.6,0.5),2,2,byrow=TRUE)

# Set time step and initial values for the system
time=100
L.array=array(0,c(2,6,time));
L.array[,,1]=c(2,recursive=TRUE);
p=list()

par(mfrow=c(2,3))
for (i in seq(1,6,1)){
	E_values=round(eigen(M.array[,,i])$values,2)
	for (j in seq(1,time-1,1)){
		L.array[,i,j+1]<-M.array[,,i]%*%L.array[,i,j]
		}
	plot(L.array[1,i,],L.array[2,i,],type='l',col='red',xlab='Romeo',ylab='Juliet',main=paste("lambdas = ",E_values[1],"&",E_values[2]))
	points(L.array[1,i,1],L.array[2,i,1],type='p')
}
