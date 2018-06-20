#CECS 552 Programming Assignment 1, Fall 2016


#EXERCISE 1:


#This function returns the sum of two die rolls
sample11 = function()
{
  #x : value for first dice roll
  x=sample(1:6,1) 
  cat("\nThe result of the first dice roll is",x,"\n")
  #y : value for second dice roll
  y=sample(1:6,1)
  cat("\nThe result of the second dice roll is",y,"\n")
  #z : sum of values on both die
  z=x+y
  cat("\nThe sum of both die rolls is",z,"\n")
  return(z)
}

craps1 = function()
{
  x=sample11()
  #If first dice roll is 7 or 11,the player wins
  if(x==7  || x==11)
    return(1)
  #if first die roll is 2,3 or 12,the player looses
  else if(x==2 || x==3 || x==12)
    return(0)
  #if the first die roll is 4,5,6,8,9 or 10,thr program enters this else loop
  else
  {
    #The die are rolled again
    y=sample11()
    #Die are continued to be rolled if its sum is 2,3,12 or 11
    #while(y==2 || y==3 || y==12 || y==11)
    #y=sample11()
    #Once the die rolls result to a 7,the player looses
    while(!(y %in% c(7,x)))
      y=sample11()
    if(y==7)
      return(0)
    #Once the die rolls result to a 4,5,6,8,9 or 10,the player wins
    else if(y==4 || y==5 || y==6 || y==8 || y==9 || y==10)
      return(1)
  }
}






















#EXERCISE 2 and 3:

#This function returns the sum of two die rolls
sample1 = function()
{
  x=sample(1:6,1) 
  y=sample(1:6,1)
  z=x+y
  return(z)
}

#This is the function that is called by estimate_bernouli for random input set of 0's and 1's
craps = function()
{
  x=sample1()
  #If first dice roll is 7 or 11,the player wins
  if(x==7  || x==11)
    return(1)
  #if first die roll is 2,3 or 12,the player looses
  else if(x==2 || x==3 || x==12)
    return(0)
  #if the first die roll is 4,5,6,8,9 or 10,thr program enters this else loop
  else
  {
    #The die are rolled again
    y=sample1()
    #Die are continued to be rolled if its sum is 2,3,12 or 11
    while(!(y %in% c(7,x)))
      y=sample1()
    if(y==7)
      return(0)
    #Once the die rolls result to a 4,5,6,8,9 or 10,the player wins
    else if(y==4 || y==5 || y==6 || y==8 || y==9 || y==10)
      return(1)
  }
}

#Function to implement the formula given in the queston
sample2 = function(n,delta,calculate)
{
  sm=0
  smsq=0
  for(i in 1:n)
  {
    x=craps()
    sm=sm+x
    smsq=smsq+x*x
  }
  lambda=sm/n #Calculating mean of data
  sigmasq=(smsq-(lambda*lambda*n))/(n-1) 
  serr=sqrt(sigmasq/n) #Standard Error
  rerr=serr/lambda #Relative Error
  qdelta=qnorm((1+delta)/2) #Inverse of CDF
  #Calculating the range between which lamda falls
  left_limit=lambda-qdelta*serr 
  right_limit=lambda+qdelta*serr
  calculate=qdelta * serr #LHS of the equation given in exercise 2
  cat("\nThe length of input set is",n)
  cat("\nCalculated LHS of the formula is :",calculate)
  cat("\nConfidence Interval :-",left_limit," - ",right_limit)
  cat("\nlambda :-",lambda)
  cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  return(calculate)
}

estimate_bernoulli=function(delta,epsilon,craps)
{
  calculate=1
  p=1
  n=10000
  sample2(n,delta,calculate) #Initially passing n sample data
  #The number of samples keep incrementing by 10000 until the equation is satisfied
  while(p>epsilon)
  {
    n=n+10000
    p=sample2(n,delta,calculate)
  }
  cat("\nHence as the size of the input set increases,the confidence interval decreases")
}

























#EXERCISE 4:

network_reliability<-function()
{
  flag=1
  n=0
  prob_sum=0
  sigsquare=0
  lambDa=0
  #Iterate the loop until 50 failures
  while(flag<=50)
  {
    #n:Total of all the configurations 
    n=n+1
    #Creating the random input of 7 edges
    ss=sample(c(0.999,0.001), 7, replace = TRUE) 
    
    #Failure of the edges:1 and 2
    if(ss[1]==0.001 && ss[2]==0.001)
    {
      prob_sum=prob_sum+prod(ss)
      flag=flag+1
    }
    
    #Failure of the edges:5,6 and 7
    else if(ss[5]==0.001 & ss[6]==0.001 & ss[7]==0.001)
    {
      prob_sum=prob_sum+prod(ss)
      flag=flag+1
    }
    
    #Failure of the edges:2,4,6 and 5
    else if(ss[2]==0.001 & ss[4]==0.001 & ss[6]==0.001 & ss[5]==0.001)
    {
      prob_sum=prob_sum+prod(ss)
      flag=flag+1
    }
    
    #Failure of the edges:1,3,4 and 7
    else if(ss[1]==0.001 & ss[3]==0.001 & ss[4]==0.001 & ss[7]==0.001)
    {
      prob_sum=prob_sum+prod(ss)
      flag=flag+1
    }
    
    #Failure of the edges:3,4,5  and 7
    else if(ss[3]==0.001 & ss[4]==0.001 & ss[5]==0.001 & ss[7]==0.001)
    {
      prob_sum=prob_sum+prod(ss)
      flag=flag+1
    }
  }
  lambDa=prob_sum/n
  sigmasq=(50*lambDa-(lambDa*lambDa))
  cat("Total number of configurations before failing 50 times:: ",n,"\n")
  cat("Lambda:: ",lambDa,"\n")
  cat("Variance is:: ",sigmasq,"\n")
}



#EXERCISE 5
network_reliability2<-function()
{
  p=0.999
  q=5/7
  flag=0
  n=0
  prob=0
  phiRatio=0
  lambDa=0
  
  #Iterate the loop until 50 failures
  while(flag<50)
  {
    #n:Total of all the configurations 
    n=n+1
    #Creating the random input of 7 edges with new probability given to the edges
    ss=sample(c(5/7,2/7), 7, replace = TRUE)
    #Failure of the edges:1 and 2
    if(ss[1]==2/7 & ss[2]==2/7)
    {
      flag=flag+1
      prob=prob+prod(ss)
      
    }
    
    #Failure of the edges:5,6 and 7
    else if(ss[5]==2/7 & ss[6]==2/7 & ss[7]==2/7)
    {
      flag=flag+1
      prob=prob+prod(ss)
      
    }
    
    #Failure of the edges:2,4,6 and 5
    else if(ss[2]==2/7 & ss[4]==2/7 & ss[6]==2/7 & ss[5]==2/7)
    {
      flag=flag+1
      prob=prob+prod(ss)
    }
    
    #Failure of the edges:1,3,4 and 7
    else if(ss[1]==2/7 & ss[3]==2/7 & ss[4]==2/7 & ss[7]==2/7)
    {
      flag=flag+1
      prob=prob+prod(ss)
      
    }
    
    #Failure of the edges:3,4,5  and 7
    else if(ss[3]==2/7 & ss[4]==2/7 & ss[5]==2/7 & ss[7]==2/7)
    {
      flag=flag+1
      prob=prob+prod(ss)
    }
  }
  phiRatio=(p/q)^5*((1-p)/(1-q))^2
  lambDa=prob/n * phiRatio
  sigmasq=(50*(phiRatio*phiRatio)-(lambDa*lambDa*n))/(n-1)
  cat("Total number of configurations until 50 failures:: ",n,"\n")
  cat("Lambda Hat:: ",lambDa,"\n")
  cat("Variance is:: ",sigmasq,"\n")
}

