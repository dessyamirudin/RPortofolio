# EM ALGORITHM FOR SIMPLE 1D GAUSSIAN MIXTURE MODEL.
#
# The arguments are the data vector, the number of components to use, the
# number of EM iterations to do, and a matrix of initial responsibilities 
# of components for data items (default is a random assignment), and whether
# to print trace information (default FALSE).
#
# The value returned is a list with elements pi (probabilities of components),
# mu (means of components), sigma (standard deviations of components), and
# r, the matrix of responsibilities (which could be passed to another call
# of mix.em).

#x = traindata
#K = number of component
#iters = number of iteration


#calculating the likelihood
lkhood<-function (thetha,x)
{
	likelihood<-1
	for (i in 1:n)
	{
		likelihood<-likelihood*thetha^x[i]*(1-thetha)^(1-x[i])
	}
	
	return (likelihood)
}

#returning the q value (0 or 1)
get.q<-function(K,y)
{
	q<-matrix(0,10*K) #the value of q (0 or 1)
	elemenKy<-rep[NA,K]
	
	for (k in 1:K)
	{
		elemenKy[k]<-K*yind+k
	}
	
	for (k in 1:10*K)
	{
		if (match(k,elemenKy))
		{
			q[k]<-1
		}
	}
	
	return(q)
}

#main function to calculate the digit Expectation Maximization
digit.em <- function (x, y, K,alpha, trace=FALSE)
{ 
  Y<-c(0,1,2,3,4,5,6,7,8,9)
  numberclass<-10
  j<-length(x) #number of pixel
  n<-nrow(x) #number of data
  Pyx<-matrix(NA,n,numberclass) #probability of number
  q<-matrix(0,10*K) #the value of q (0 or 1)
  r = matrix(runif(K*length(x),1,2),length(x),K)
    
  # Do EM iterations, starting with M step, then E step.

  thetha <- matrix(NA,K,j)
  
  for (data in 1:n)
  {
	x_i<-x[data,]
	y_i<-y[data]+1 #augment the data by one to ease the indexing because indexing in R start from 1 (not zero). In the end of classification, just substract each class by 1
	N <- length(x_i)
		
	if (nrow(r)!=N || ncol(r)!=K)
	{ 
		stop("Matrix of responsibilities is the wrong size")
	}

	# Make sure initial responsibilities sum to one.
    for (i in 1:N)
	{ 
		r[i,] <- r[i,] / sum(r[i,])
	}
	
	mu <- rep(NA,K)
    sigma <- rep(NA,K)
  
	rt <- colSums(r)
  
	#M step
	pi <- rt / sum(rt)
	for (k in 1:K) 
	{ 
		mu[k] <- sum(r[,k]*x_i) / rt[k]
		sigma[k] <- sqrt (sum(r[,k]*(x_i-mu[k])^2) / rt[k])
	}
	
	sumnom<-rep(NA,n)
	sumdenom<-rep(NA,n)
	for (k in 1:K) 
	{ 
		for (jind in 1:j)
		{
			for (nind in 1:n)
				{
				   sumnom[nind]<-r[nind,k]*x[nind,jind]
				   sumdenom[nind]<-r[nind,k]
				}
			thetha[k,jind]<-(alpha+sum(sumnom))/(2*alpha+sum(sumdenom))
		} 
			  
	}
	
	for (t in 1:iters)
	{
		if (trace)
		{ cat("\nr:\n")
			print(round(r,3))
		}

		rt <- colSums(r)
		
		# E step.	
		#updating (small) q
		
		q<-get.q(K,y_i)
		#calculatinglikelihood
		likelihood<-lkhood(thetha,x_i)
		pi.q.lkhood=pi*q*likelihood
		#probability for that particular number is >0
		Pyx[data][y_i]=sum(pi.q.lkhood)
		#probability for any other number is equal to 0
		for (yindex in 1:numberclass)
		{
			if (yindex!=(y_i-1))
			{
				Pyx[data][yindex]<-0
			}
		}
			 
		for (i in 1:N)
		{ 
			r[i,] <- pi * dnorm(x[i],mu,sigma)
			r[i,] <- r[i,] / sum(r[i,]) 
		}
			
		# M step.

		pi <- rt / sum(rt)
		for (k in 1:K) 
		{ 
			mu[k] <- sum(r[,k]*x_i) / rt[k]
			sigma[k] <- sqrt (sum(r[,k]*(x_i-mu[k])^2) / rt[k])
		}
			
		sumnom<-rep(NA,n)
		sumdenom<-rep(NA,n)
		for (k in 1:K) 
		{ 
			for (jind in 1:j)
			{
				for (nind in 1:n)
				{
				   sumnom[nind]<-r[nind,k]*x[nind,jind]
				   sumdenom[nind]<-r[nind,k]
				}
			thetha[k,jind]<-(alpha+sum(sumnom))/(2*alpha+sum(sumdenom))
			} 
		}

		if (trace)
		{ 
			cat ("\npi:", round(pi,3), "  mu:", round(mu,3),
			"  sigma:", round(sigma,3), "\n")
		}	
	}
}
  list (pi=pi, thetha=thetha, r=r)
}

