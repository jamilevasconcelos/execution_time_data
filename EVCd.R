#####################################################################################
##                                                                                 ##
##     Welcome to use this package to calculate the maximum likelihood / moment    ##
##     estimation for the extreme value index, or to test one dimentional extreme  ##
##     value condition.                                                            ##
##                                                                                 ##
##     All rights are reserved by Deyuan Li and Juerg Huesler.                     ##
##                                                                                 ##
##     Latest edition is Jan. 05, 2006.                                            ##
##                                                                                 ##
##     For more details on how to use this package see read_TestEVC1d.pdf          ##
##                                                                                 ##
#####################################################################################



################################################################
#                                                              #
# The functions: h, L, h1, h2, rtbisection, and rtnewton,      #
# are used for the function MLGPDtheta().                      #
#                                                              #
################################################################

# h:=dL(theta;x)/d(theta). See Grimshaw (1993).
h=function(list,n, k, theta)
{
  # X has been sorted, n=length(list), and k is an integer.
  sum1=0.0;
  sum2=0.0;
  for(i in 1:k)
  {
    sum1=sum1+log(1-theta*(list[n+1-i]-list[n-k]));
    sum2=sum2+1.0/(1-theta*(list[n+1-i]-list[n-k]));
  }
  sum1=sum1/k;
  sum2=sum2/k;
  value=(1.0+sum1)*sum2-1.0;
  return(value);
} # end of the function h()

# Calculate the likelyhood function value. see Grimshaw (1993).
L=function(X,n,k,gamma, alpha)
{  
  # X has been sorted, n=length(X), and k is an integer.
  sum=0.0;
  for(i in 1:k)
  {    sum=sum+log(1+gamma/alpha*(X[n+1-i]-X[n-k]));}
  value=-k*log(alpha)-sum*(1.0+1.0/gamma);
  return(value);
}

# h1:=dh/d(theta). See Grimshaw (1993).
h1=function(X,n,k,theta)
{
  # X has been sorted, n=length(X), k is an integer.
  sum1=0.0;
  sum2=0.0;
  sum3=0.0;
  for(i in 1:k)
  {
    sum1=sum1+(1-theta*(X[n+1-i]-X[n-k]))^(-2.0);
    sum2=sum2+(1-theta*(X[n+1-i]-X[n-k]))^(-1.0);
    sum3=sum3+log(1-theta*(X[n+1-i]-X[n-k]));
  }
  value=1.0/theta*(sum1/k-(sum2/k)^(2.0)-(sum3/k)*(sum2/k-sum1/k));
  return(value);
}

# h2:= limit of dh1/d(theta) as theta goes to 0. See Grimshaw (1993).
h2=function(X,n,k)
{
  # X has been sorted, n=length(X), and k is an integer.
  sum=0.0;
  average=0.0;
  for(i in 1:k)
  {
    sum=sum+(X[n+1-i]-X[n-k])*(X[n+1-i]-X[n-k]);
    average=average+(X[n+1-i]-X[n-k]);
  }
  value=sum/k-2*(average/k)^(2.0);
  return(value);
}

# find root of h() by bisection method. See Numerical Recipes in C, page 353.
rtbisection=function(x1, x2, X, n,k)
{
  # X has been sorted, n=length(X), and k is an integer.
  maxit=500;
  f=h(X,n,k,x1);
  fmid=h(X,n,k,x2);
  if(f*fmid>=0.0)
  {
    # cat("\n","There is no zero root in this interval.","\n");
    return(0.0);
  }
  if(f<0.0)
  {  dx=x2-x1; rtb=x1;}
  else
  {  dx=x1-x2; rtb=x2;}
  for(i in 1:maxit)
  {
    dx=dx*0.5;
    xmid=rtb+dx;
    fmid=h(X,n,k,xmid);
    if(fmid<=0.0){      rtb=xmid;}
    if(abs(dx)<0.000001 | abs(fmid)<0.000001){ return(rtb)};
  }
  return(0.0);
}

# find root of h() by newton method.
rtnewton=function(x1,x2,x0,X,n,k)
{
  # X has been sorted, n=length(X), and k is an integer;
  maxit=500;
  rtn=x0;
  for(i in 1:maxit)
  {
    f=h(X,n,k,rtn);
    df=h1(X,n,k,rtn);
    dx=f/df;
    rtn=rtn-dx;
    if((x1-rtn)*(rtn-x2)<0.0) {return(0.0);}
    if(abs(dx)<0.000001)      {return(rtn);}
  }
  return(0.0);
}

# calculate the maximum likelihood estimator of gamma via theta. See Grimshaw (1993).
cgamma=function(X,n,k, theta)
{
  # X has been sorted, n=length(X), and k is an integer.
  value=0.0;
  for(i in 1:k)
  {  value=value+log(1-theta*(X[n+1-i]-X[n-k]));}
  return(value/k);
}


# Calculate the ML-estimator of theta for GPD distribution. See Grimshaw(1993). 
# Latest edition on Jan. 04, 2006.
MLGPDtheta=function(X,n,k=0)
{
  # X is the sample, and has been sorted.  
  # k could be a sequence integers or an integer. If k=0 (default value), we will use default sequence as below.
  # n=length(X);
  
  large=99999999.0;    # define the default value for infinity
  epsilon0=0.000001;   # define the default value for zero
  maxit1=100;          # set the maximum iterative time 
  maxit2=200;          # set another maximum iterative time
  
  if(length(k)==1 && k==0)
  {
    if(n/5> 5000){ increasek=50; }
    else if(n/5 > 2000) {increasek=20;}
    else if(n/5 > 1000) {increasek=10;}
    else { increasek=5;}
    k=seq(10,n/5,by=increasek);
  }
  sizek=length(k);
  
  theta=numeric(sizek); # define a vector which will be returned
  
  for(j in 1:sizek)
  {
    if(k[j]<5 |k[j]>n-5)     {theta[j]=NA;}
    else  # 5<=k[j]<=n-5
    {
      average=0.0;
      for(i in 1:k[j])
      {  average=average+(X[n+1-i]-X[n-k[j]]);}
      average=average/k[j];
      epsilon=epsilon0/max(average,1.0);
      if(X[n+1-k[j]]-X[n-k[j]]>epsilon0)
      {   thetalow=2*(X[n+1-k[j]]-X[n-k[j]]-average)/((X[n+1-k[j]]-X[n-k[j]])^2);}
      else
      {   thetalow=-large+1;}
      thetaup=1.0/(X[n]-X[n-k[j]])-epsilon;
      
      sign=h2(X,n,k[j]);
      if (sign>0)   # that means only one root in every interval
      {
        thetalow2=large;
        thetaup2=large;
        temp=thetalow;
        initial=temp/2.0;
        i=1;
        while((i<maxit1) & (h(X,n,k[j],temp)*h(X,n,k[j],initial)>0.0))
        {
          i=i+1;
          temp=initial;
          initial=temp/2.0;
        }
        if((i==maxit1) | (initial > -epsilon)) {   thetalow1=large;}
        else {   thetalow1=rtbisection(temp,initial,X,n,k[j]);}
        temp=thetaup;
        initial=temp/2.0;
        i=1;
        while((i<maxit1) & (h(X,n,k[j],temp)*h(X,n,k[j],initial) > 0.0))
        {
          i=i+1;
          temp=initial;
          initial=temp/2.0;
        }
        if((i==maxit1) | (initial< epsilon)) {thetaup1=large;}
        else {thetaup1=rtbisection(initial,temp,X,n,k[j]);}
        
        if(abs(thetalow1)< epsilon)        { thetalow1=large;}
        if(abs(thetaup1)< epsilon)         { thetaup1=large;}
      }  # end(sign>0)
      
      else # sign<0  that means there are 0 or 2 roots in every interval.
      {
        findroot1=0;
        findroot2=0;
        for(i in 1:maxit2)
        {
          temp=-0.05*i-epsilon;
          if(temp <thetalow){i=maxit2+2;}
          if(h(X,n,k[j],temp)>epsilon0)
          {
            i=maxit2+2;   findroot1=1;
            thetalow1=rtbisection(thetalow,temp,X,n,k[j]);
            thetalow2=rtbisection(temp,-epsilon,X,n,k[j]);
          }
        }
        for(i in 1:maxit2)
        {
          temp=epsilon+i*(thetaup-epsilon)/500.0;
          if(temp >thetaup) {i=maxit2+2;}
          if(h(X,n,k[j],temp)>epsilon0)
          {
            i=maxit2+2; findroot2=1;
            thetaup1=rtbisection(epsilon,temp,X,n,k[j]);
            thetaup2=rtbisection(temp,thetaup,X,n,k[j]);
          }
        }
        
        # do on the interval (thetalow, -epsilon)
        temp=thetalow;
        i=1;
        while((findroot1==0) & (i<maxit1) & (temp < epsilon*(-1)))
        {   initial=temp/2.0;
        if((temp< epsilon*(-1)) & (initial < epsilon*(-1)) & (h(X,n,k[j],temp)*h(X,n,k[j],initial)<0.0))
        {
          thetalow1=rtbisection(temp,initial,X,n,k[j]);
          thetalow2=rtbisection(initial,-epsilon,X,n,k[j]);
          i=maxit1+2;   findroot1=1;
        }
        else
        {
          thetalow1=rtnewton(thetalow,-epsilon,temp,X,n,k[j]);
          if(thetalow1==0.0)  # out of bracket or no zero root
          { temp=initial;}
          else if(thetalow1+epsilon<(-epsilon0)) # find one root
          {
            if(h1(X,n,k[j],thetalow1)>0)
              # the other zero root is on [thettalow1,-epsilon]
            { thetalow2=rtbisection(thetalow1+epsilon,-epsilon,X,n,k[j]);}
            else # the other root is on [thetalow, thetalow1]
            { thetalow2=rtbisection(thetalow,thetalow1-epsilon,X,n,k[j]);}
            i=maxit1+2; findroot1=1;
          }
        }
        i=i+1;
        } # end while
        if(findroot1==0 )  # there are no root in (thetalow, -epsilon)
        {
          thetalow1=large;
          thetalow2=large;
        }
        if(abs(thetalow1)< epsilon)        { thetalow1=large;}
        if(abs(thetalow2)< epsilon)        { thetalow2=large;}
        
        # do on the interval (epsilon, thetaup)
        temp=thetaup;
        i=1;
        while((findroot2==0) & (i<maxit1) & (temp > epsilon))
        {   initial=temp/2.0;
        if((temp>epsilon) & (initial>epsilon) & (h(X,n,k[j],temp)*h(X,n,k[j],initial)<0.0))
        {
          thetaup1=rtbisection(initial,temp,X,n,k[j]);
          thetaup2=rtbisection(epsilon, initial,X,n,k[j]);
          i=maxit1+2; findroot2=1;
        }
        else
        {
          thetaup1=rtnewton(epsilon,thetaup,temp,X,n,k[j]);
          if(thetaup1==0.0)  # out of bracket or no zero root
          { temp=initial;}
          else if(thetaup1 > epsilon) # find one root
          {
            if(h1(X,n,k[j],thetaup1)>0.0)
              # the other zero root is on [thetaup1,thetaup]
            { thetaup2=rtbisection(thetaup1+epsilon0,thetaup,X,n,k[j]);}
            else # the other root is on [epsilon,thetaup1]
            { thetaup2=rtbisection(epsilon,thetaup1-epsilon,X,n,k[j]);}
            i=maxit1+2;  findroot2=1;
          }
        }
        i=i+1;
        } # end while
        
        if(findroot2==0)  # there are no root in (epsilon,thetaup)
        {
          thetaup1=large;
          thetaup2=large;
        }
        
        if(abs(thetaup1)< epsilon)   { thetaup1=large;}
        if(abs(thetaup2)< epsilon)   { thetaup2=large;}
      } # end (sign<=0)
      
      
      if(thetalow1<(large-1))   # thetalow1 is the zero solusion of h().
      {
        r1=cgamma(X,n,k[j],thetalow1);
        L1=L(X,n,k[j],r1,-r1/thetalow1);
      }
      else {L1=-k[j]*log(max(X[n]-X[n-k[j]],epsilon0));}
      if(thetalow2<(large-1))   # thetalow2 is the zero solusion of h().
      {
        r2=cgamma(X,n,k[j],thetalow2);
        L2=L(X,n,k[j],r2,-r2/thetalow2);
      }
      else {L2=-k[j]*log(max(X[n]-X[n-k[j]],epsilon0));}
      if(thetaup1<(large-1))   # thetaup1 is the zero solusion of h().
      {
        r3=cgamma(X,n,k[j],thetaup1);
        L3=L(X,n,k[j],r3,-r3/thetaup1);
      }
      else {L3=-k[j]*log(max(X[n]-X[n-k[j]],epsilon0));}
      if(thetaup2<(large-1))   # thetaup2 is the zero solusion of h().
      {
        r4=cgamma(X,n,k[j],thetaup2);
        L4=L(X,n,k[j],r4,-r4/thetaup2);
      }
      else {L4=-k[j]*log(max(X[n]-X[n-k[j]]),epsilon0);}
      
      index=1;
      maxL=L1;
      if(maxL<L2) { index=2; maxL=L2;}
      if(maxL<L3) { index=3; maxL=L3;}
      if(maxL<L4) { index=4; maxL=L4;}    
      
      if(maxL<= -k[j]*log(X[n]-X[n-k[j]])) {theta[j]=0.99/(X[n]-X[n-k[j]]);}
      else if(index==1)              { theta[j]= thetalow1;}
      else if(index==2)              { theta[j]= thetalow2;}
      else if(index==3)              { theta[j]= thetaup1;}
      else                           { theta[j]= thetaup2;}
    } # end else(5<=k[j]<=n-5)
  } #end for(j in 1:length(k))
  
  #if(sizek==1)     {return(theta[1]);}
  #else             {return(theta);}
  return(theta);
} # end of the function MLGPDtheta()


# Calculate the p-confidence interval for the maximum likelihood (ML) estimator of the extreme value index (EVI)
# in case of gamma>-0.50. For more details see Drees, Ferreira, de Haan (2004).
ConfMLgamma=function(X,n,k,gamma,p)
{
  # X is the sample and has been sorted. n=length(X).
  # k and gamma could be vectors or real numbers.
  # gamma is the ML-estimator for EVI, and it must > -0.5 !
  # p is the confidence level. p must be set to be 0.90, 0.95, or 0.99.
  
  sizek=length(k);
  upgamma=numeric(sizek);
  lowgamma=numeric(sizek);
  
  ## The following is to calculate upgamma[1:length(k)] and lowgamma[1:length(k)]
  #           gamma, q0.005, q0.025, q0.05,  q0.95, q0.975, q0.995
  qq=array(c(  2.0,  -9.064, -5.821, -4.864, 5.031, 5.809, 7.490,  # gamma=2.0
               1.8,  -8.407, -5.464, -4.587, 4.674, 5.257, 6.972,  # gamma=1.8
               1.6,  -7.679, -5.045, -4.238, 4.315, 4.872, 6.467,  # gamma=1.6
               1.4,  -6.950, -4.585, -3.905, 3.952, 4.592, 6.026,  # gamma=1.4
               1.2,  -6.219, -4.196, -3.579, 3.670, 4.287, 5.522,  # gamma=1.2
               1.0,  -5.651, -3.868, -3.265, 3.302, 3.942, 5.008,  # gamma=1.0
               0.8,  -4.932, -3.518, -2.939, 2.936, 3.511, 4.533,  # gamma=0.8
               0.6,  -4.258, -3.103, -2.600, 2.678, 3.058, 3.983,  # gamma=0.6
               0.4,  -3.519, -2.756, -2.242, 2.341, 2.691, 3.437,  # gamma=0.4
               0.2,  -2.838, -2.351, -1.897, 1.988, 2.274, 2.722,  # gamma=0.2
               0.0,  -2.404, -1.815, -1.541, 1.583, 1.864, 2.308,  # gamma=0.0
               -0.2,  -1.932, -1.265, -1.077, 1.143, 1.327, 1.729,  # gamma=-0.2
               -0.4,  -1.277, -0.988, -0.785, 0.840, 1.032, 1.268,  # gamma=-0.4
               -0.499,-1.327, -0.972, -0.824, 0.842, 0.970, 1.351), # gamma=-0.499
           dim=c(7,14));
  # note that qq[1][1]=2, qq[1][3]=-5.821, qq[1][6]=5.809, which are the 2.5% quantile 
  # and the 97.5% quantile of the limiting random variable of root(k)*(hat(gamma)-2) in case of ML-estimation. 
  # This table is obtained by Deyuan Li.
  q=array(dim=c(14,7));
  for(i in 1:14)
    for(j in 1:7)
      q[i,j]=qq[j,i];
  
  for(j in 1:sizek)
  {
    row=1;
    for(i in 1:length(q[,1]))
    { if(gamma[j]< q[i,1]){row=i+1;}}
    if(row==1)
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+q[row,7]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,2]/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+q[row,6]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,3]/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+q[row,5]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,4]/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
    else if(row>1 & row<=length(q[,1]))
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+(q[row,7]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,7]-q[row,7]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,2]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,2]-q[row,2]))/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+(q[row,6]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,6]-q[row,6]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,3]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,3]-q[row,3]))/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+(q[row,5]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,5]-q[row,5]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,4]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,4]-q[row,4]))/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
    else
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+q[row-1,7]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,2]/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+q[row-1,6]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,3]/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+q[row-1,5]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,4]/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
  }# end for(j in 1:length(q[,1]))   
  
  return(cbind(lowgamma,upgamma));
} # end of the function ConfMLgamma()


# Calculate the test statistics of (2.7) in Drees, de Haan and Li(2006).
MLTest=function(X, n, k, rn, an, bn, eta)
{
  # Here, X has been sorted, and n,k,rn,an, bn and eta are one dimensional real parameters
  # eta must be set to be 0.5,1.0, or 2.0.
  num_interval=5000;
  x=0.0;x1=0.0;
  value=0.0;
  
  index=n-1;
  for(i in 1:num_interval)
  {
    x=(i+0.5)/num_interval;
    x1=an * (x^(-rn)-1)/rn +bn; #note that x1>=bn and x1=bn iff x=1
    findroot=0;
    leftindex=n-k-1;
    rightindex=index+1;
    
    if(x1>=X[n]) {findroot=1;index=n;}
    while(findroot==0)
    {
      index=floor((leftindex+rightindex)/2);
      if(x1<X[index])  {    rightindex=index;}
      else             {    leftindex=index;}
      if(rightindex-leftindex <= 1)
      {
        findroot=1;
        index=leftindex;
      }
    } #end while(findroot==0)
    
    Fn=(index + 0.0)/n;  # calculate Fn(x1)
    temp=(n*1.0/k*(1-Fn)-x)^2;
    value=value+temp/(x^(2.0-eta));
  }
  value=value*k*1.0/num_interval;
  return (value);
}


# Calculate the quantiles of the limiting random variable for the ML-estimator of gamma.
# For more details see Drees, de Haan and Li (2006).
MLQuanOfLimRV=function(gamma,eta)
{
  # Here, gamma and eta are one dimensional real parameters
  # eta must be set to be 0.5, 1.0, or 2.0.
  
  qgamma=numeric(length=6); # define a vector which will be returned
  num=14;                   # the number of gamma's
  vr=c(4.0,3.0,2.0, 1.5, 1.0, 0.5, 0.25, 0.0, -0.1,-0.2, -0.3, -0.4, -0.45, -0.499);
  q=array(dim=c(num,6));
  
  # q1 is the quantile table for eta=1.0
  q1=array(c(          0.637736,0.462287,0.393035,0.073976,0.065299,0.051213, #gamma=4
                       0.605360,0.440199,0.371574,0.072978,0.064026,0.051379, #gamma=3
                       0.553049,0.402332,0.344111,0.071288,0.063066,0.050585, #gamma=2
                       0.537605,0.400187,0.340403,0.070677,0.062072,0.049742, #gamma=1.5
                       0.529121,0.388239,0.329951,0.070550,0.062116,0.050051, #gamma=1
                       0.535751,0.404079,0.342858,0.071302,0.062945,0.050442, #gamma=0.5
                       0.567699,0.414629,0.355150,0.073126,0.064685,0.050733, #gamma=0.25
                       0.617074,0.454687,0.388294,0.076028,0.066700,0.052428, #gamma=0
                       0.639360,0.470576,0.400316,0.077401,0.068041,0.053568, #gamma=-0.1
                       0.683520,0.499865,0.424680,0.079203,0.069078,0.054647, #gamma=-0.2
                       0.732664,0.530803,0.449027,0.081684,0.071623,0.055692, #gamma=-0.3
                       0.780575,0.575960,0.483735,0.084972,0.074208,0.057646, #gamma=-0.4
                       0.838482,0.604720,0.510834,0.087060,0.075747,0.058039, #gamma=-0.45
                       0.908641,0.799170,0.545836,0.090675,0.079175,0.061499),#gamma=-0.499
           dim=c(6,num));
  # For example q[1][1]=0.637736, that is 0.995 quantile for gamma=4;
  #             q[6][14]=0.061499, that is 0.005 quantile for gamma=-0.499.
  
  # q2 is the quantile table for eta=2.0
  q2=array(c(          0.205992,0.147396,0.123117,0.020905,0.018180,0.014198, #gamma=4
                       0.188392,0.135473,0.114317,0.020717,0.018018,0.013844, #gamma=3
                       0.177860,0.130883,0.110532,0.020130,0.017671,0.013834, #gamma=2
                       0.175828,0.129866,0.108714,0.020275,0.017794,0.013899, #gamma=1.5
                       0.185472,0.135295,0.113747,0.020772,0.018150,0.014177, #gamma=1
                       0.200261,0.146624,0.123305,0.021451,0.018766,0.014655, #gamma=0.5
                       0.221364,0.159149,0.133236,0.022487,0.019486,0.014877, #gamma=0.25
                       0.257523,0.182617,0.151886,0.023724,0.020470,0.015541, #gamma=0
                       0.274180,0.190292,0.159707,0.024545,0.021069,0.016272, #gamma=-0.1
                       0.294604,0.207571,0.170932,0.024964,0.021440,0.016356, #gamma=-0.2
                       0.310172,0.217808,0.180884,0.025552,0.021929,0.016822, #gamma=-0.3
                       0.342870,0.238619,0.196749,0.026786,0.023115,0.017495, #gamma=-0.4
                       0.362309,0.254364,0.207666,0.027332,0.023486,0.017713, #gamma=-0.45
                       0.395850,0.267598,0.220146,0.028121,0.023946,0.018043),#gamma=-0.499
           dim=c(6,num));
  
  # q05 is the quantile table for eta=0.5
  q05=array(c(         1.943625,1.460665,1.243053,0.260115,0.228530,0.182628, #gamma=4
                       1.905226,1.414860,1.209064,0.257532,0.226380,0.182701, #gamma=3
                       1.770277,1.324236,1.140974,0.249662,0.220238,0.176986, #gamma=2
                       1.701908,1.258430,1.082257,0.246487,0.217749,0.174701, #gamma=1.5
                       1.609785,1.210013,1.047219,0.243171,0.215172,0.173073, #gamma=1
                       1.534984,1.163986,1.005267,0.238500,0.211773,0.168013, #gamma=0.5
                       1.554401,1.169179,1.010400,0.238809,0.212244,0.169772, #gamma=0.25
                       1.577672,1.188464,1.030369,0.242113,0.215612,0.171133, #gamma=0
                       1.615728,1.227170,1.059353,0.243629,0.215769,0.174243, #gamma=-0.1
                       1.683208,1.281185,1.107293,0.250161,0.220863,0.176611, #gamma=-0.2
                       1.815005,1.351037,1.159174,0.255918,0.226946,0.179973, #gamma=-0.3
                       1.929807,1.448943,1.244266,0.264883,0.233593,0.185496, #gamma=-0.4
                       2.114046,1.546862,1.322408,0.275091,0.243071,0.192483, #gamma=-0.45
                       2.390001,1.758322,1.490705,0.296127,0.258800,0.201029),#gamma=-0.499
            dim=c(6,num));
  
  if(eta==1.0)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q1[j,i];
  }
  
  else if(eta==0.5)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q05[j,i];
    
  }
  else if(eta==2.0)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q2[j,i];
  }
  else
  {
    cat("No quantile table for eta=",eta, "!\n");
    cat("eta must be among {0.5, 1, 2} !!\n");
    return;
  }
  
  
  for(i in 1:num)
  {
    if(i==1 && gamma>= vr[i])
    {
      for (j in 1:6)
        qgamma[j]=q[i,j];
      i=num+1;
    }
    else if(i>1 && i< num && gamma< vr[i-1] && gamma>= vr[i])
    {
      
      for (j in 1:6)
        qgamma[j]=q[i,j]+(gamma-vr[i])/(vr[i-1]-vr[i])*(q[i-1,j]-q[i,j]);
      i=num+1;
    }
    else if(i== num && gamma < vr[i])
    {
      for (j in 1:6)
        qgamma[j]=q[i,j];
      i=num+1;
    }
  }
  return(qgamma);    
} # end this function




################################################################
##                                                            ##
## The following functions are used for Moment estimation     ##
## For more detail see Dekkers, de Haan and Einmahl (1989),   ##
## and Dietrich, de Haan and Huesler (2002).                  ##
##                                                            ##
################################################################

# Moment estimator for the positive part of EVI. 
gammaP=function(SX,n,k)
{
  # SX has been sorted, n=length(SX), and k is an integer.
  M1=0.0;
  for(i in 1:k)
    M1=M1+log(SX[n+1-i])-log(SX[n-k]);
  M1=M1/k;
  return (M1);
  
}

# Monment estimator for the negative part of EVI.
gammaN=function(SX,n,k,rp)
{
  # SX has been sorted, n=length(SX), and k is an interger.
  # rp is the moment estimator for the positive part of EVI.
  M2=0.0;
  for(i in 1:k)
    M2=M2+(log(SX[n+1-i])-log(SX[n-k]))*(log(SX[n+1-i])-log(SX[n-k]));
  M2=M2/k;
  return (1-0.5/(1-rp*rp/M2));
}


# Calculate the p-confidence interval for the moment (M) estimator of the extreme value index (EVI).
# For more details see Dekkers, de Haan and Einmahl (1989).
ConfMgamma=function(X,n,k,gamma,p)
{
  # X is the sample and has been sorted. n=length(X).
  # k and gamma could be vectors or real numbers.
  # gamma is the ML-estimator for EVI, and it must > -0.5 !
  # p is the confidence level. p must be set to be 0.90, 0.95, or 0.99.
  
  sizek=length(k);
  upgamma=numeric(sizek);
  lowgamma=numeric(sizek);
  
  ## The following is to calculate upgamma[1:length(k)] and lowgamma[1:length(k)]
  #           gamma, q0.005, q0.025, q0.05,  q0.95, q0.975, q0.995  
  qq=array(c(  2.0,  -5.605, -4.241, -3.643, 3.631,  4.307,  5.763,  # gamma=2.0
               1.8,  -5.195, -3.994, -3.345, 3.291,  4.017,  5.229,  # gamma=1.8
               1.6,  -4.786, -3.647, -3.064, 3.008,  3.720,  4.695,  # gamma=1.6
               1.4,  -4.410, -3.307, -2.759, 2.734,  3.345,  4.171,  # gamma=1.4
               1.2,  -3.999, -3.015, -2.500, 2.508,  3.149,  3.740,  # gamma=1.2
               1.0,  -3.557, -2.688, -2.281, 2.289,  2.787,  3.280,  # gamma=1.0
               0.8,  -3.173, -2.457, -2.059, 2.016,  2.483,  3.129,  # gamma=0.8
               0.6,  -2.872, -2.265, -1.832, 1.818,  2.204,  2.905,  # gamma=0.6
               0.4,  -2.639, -2.103, -1.676, 1.628,  1.991,  2.670,  # gamma=0.4
               0.2,  -2.403, -1.954, -1.628, 1.487,  1.844,  2.665,  # gamma=0.2
               0.0,  -2.204, -1.883, -1.571, 1.391,  1.799,  2.523,  # gamma=0.0
               -0.2,  -2.599, -1.973, -1.696, 1.552,  1.868,  2.432,  # gamma=-0.2
               -0.4,  -3.206, -2.409, -2.076, 1.819,  2.192,  2.859,  # gamma=-0.4
               -0.6,  -4.141, -3.095, -2.558, 2.312,  2.679,  3.605,  # gamma=-0.6
               -0.8,  -5.127, -3.623, -3.094, 2.748,  3.264,  4.597,  # gamma=-0.8
               -1.0,  -6.202, -4.345, -3.781, 3.386,  4.032,  5.351,  # gamma=-1.0
               -1.2,  -7.628, -5.002, -4.495, 4.118,  4.733,  6.188,  # gamma=-1.2
               -1.4,  -8.887, -5.846, -5.242, 4.795,  5.656,  7.044,  # gamma=-1.4
               -1.6,  -9.952, -6.607, -5.971, 5.498,  6.433,  7.936,  # gamma=-1.6
               -1.8, -11.021, -7.526, -6.666, 6.253,  7.354,  8.861,  # gamma=-1.8
               -2.0, -12.091, -8.509, -7.463, 7.044,  8.280,  9.816), # gamma=-2.0
           dim=c(7,21));
  # note that qq[1][1]=2, qq[1][3]=-4.241, qq[1][6]=4.307, which are the 2.5% quantile 
  # and the 97.5% quantile of the limiting random variable of root(k)*(hat(gamma)-2) in case of moment estimation. 
  # This table is obtained by Deyuan Li.
  q=array(dim=c(21,7));
  for(i in 1:21)
    for(j in 1:7)
      q[i,j]=qq[j,i];
  
  for(j in 1:sizek)
  {
    row=1;
    for(i in 1:length(q[,1]))
    { if(gamma[j]< q[i,1]){row=i+1;}}
    if(row==1)
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+q[row,7]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,2]/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+q[row,6]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,3]/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+q[row,5]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row,4]/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
    else if(row>1 & row<=length(q[,1]))
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+(q[row,7]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,7]-q[row,7]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,2]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,2]-q[row,2]))/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+(q[row,6]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,6]-q[row,6]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,3]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,3]-q[row,3]))/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+(q[row,5]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,5]-q[row,5]))/sqrt(k[j]);
        lowgamma[j]=gamma[j]+(q[row,4]+(gamma[j]-q[row,1])/(q[row-1,1]-q[row,1])*(q[row-1,4]-q[row,4]))/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
    else
    {
      if(p==0.99)
      {
        upgamma[j]=gamma[j]+q[row-1,7]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,2]/sqrt(k[j]);
      }
      else if(p==0.95)
      {
        upgamma[j]=gamma[j]+q[row-1,6]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,3]/sqrt(k[j]);
      }
      else if(p==0.90)
      {
        upgamma[j]=gamma[j]+q[row-1,5]/sqrt(k[j]);
        lowgamma[j]=gamma[j]+q[row-1,4]/sqrt(k[j]);
      }
      else
      {upgamma[j]=NA; lowgamma[j]=NA;}
    }
  }# end for(j in 1:length(q[,1]))
  return(cbind(lowgamma,upgamma));   
} # end of the function ConfMgamma()

# Calculate the value of test statistics for Monment estimation. 
# More details see Dietrich, de Haan and Huesler (2002), formular (6) on page 74.
Mtest=function(SX,n,k, rp, rn, eta)
{
  # SX has been sorted, n=length(SX), k is an integer.
  rp0=rp;
  rn0=rn;
  NumInterval=5000;
  
  if(abs(rp0)<=0.000000001)  rp0=0.000000001;
  if(abs(rn0)<=0.000000001)  rn0=-0.000000001;
  
  v=0.0;
  for(i in 1:NumInterval)
  {
    t=(i-0.5)/NumInterval;
    temp=((log(SX[n-floor(k*t)])-log(SX[n-k]))/rp0-(t^(-rn0)-1)/rn0*(1-rn0))^2;
    v=v+temp*(t^eta);
  }
  v=v*k*1.0/NumInterval;
  return(v);
}


# Calculate the quantiles of the limiting random variabke for moment estimation.
# For more details see Dietrich, de Haan and Huesler (2002).
MQuanOfLimRV=function(gamma, eta)
{
  # Here, gamma and eta are one dimensional parameters.
  # eta must be set to be 0.5, 1.0, or 2.0.
  
  num=8;  #the number of gamma's
  vr=c(0.0, -0.1,-0.2, -0.3, -0.4, -0.5, -0.6, -0.7);
  q=array(dim=c(num,6));
  qgamma=numeric(length=6);
  
  # q1 is the quantile table for eta=1.0.
  q1=array(c(          0.598137,0.449306,0.382296,0.075071,0.066139,0.052228, #gamma>=0
                       0.524652,0.388830,0.330187,0.064150,0.056426,0.044777, #gamma=-0.1
                       0.485549,0.358561,0.303235,0.057128,0.050095,0.039418, #gamma=-0.2
                       0.464611,0.339573,0.286287,0.052965,0.046387,0.036158, #gamma=-0.3
                       0.445451,0.329290,0.276755,0.049083,0.043046,0.033549, #gamma=-0.4
                       0.442714,0.315999,0.266152,0.046181,0.040191,0.031754, #gamma=-0.5
                       0.431768,0.310651,0.261237,0.044607,0.038689,0.030142, #gamma=-0.6
                       0.428554,0.311127,0.259113,0.042737,0.037136,0.028672),#gamma=-0.7
           dim=c(6,num));
  # For example q[1][1]=0.254102, that is 0.995 quantile for gamma>=0;
  #             q[6][8]=0.015324, that is 0.005 quantile for gamma=-0.7.
  
  # q2 is the quantile table for eta=2.0
  q2=array(c(          0.254102,0.181177,0.150285,0.023622,0.020384,0.015495, #gamma>=0
                       0.238476,0.173502,0.144091,0.023081,0.019885,0.015240, #gamma=-0.1
                       0.237647,0.168834,0.140803,0.022780,0.019650,0.014985, #gamma=-0.2
                       0.236446,0.168143,0.140081,0.022738,0.019685,0.015140, #gamma=-0.3
                       0.237739,0.169381,0.141187,0.022766,0.019777,0.015280, #gamma=-0.4
                       0.237106,0.169404,0.141413,0.022753,0.019658,0.015184, #gamma=-0.5
                       0.247065,0.173481,0.143749,0.022891,0.019785,0.015187, #gamma=-0.6
                       0.249319,0.176265,0.147037,0.023140,0.019918,0.015324),#gamma=-0.7
           dim=c(6,num));
  
  # q05 is the quantile table for eta=0.5
  q05=array(c(         1.603418,1.201666,1.031165,0.240792,0.214228,0.169644, #gamma>=0
                       1.136201,0.852357,0.734825,0.163406,0.144518,0.114479, #gamma=-0.1
                       0.938664,0.699119,0.598285,0.124884,0.109999,0.087724, #gamma=-0.2
                       0.850935,0.618594,0.522603,0.101854,0.089552,0.070870, #gamma=-0.3
                       0.770397,0.559957,0.474271,0.088121,0.077223,0.060974, #gamma=-0.4
                       0.749072,0.528102,0.442153,0.078732,0.068762,0.053524, #gamma=-0.5
                       0.697168,0.499466,0.419562,0.072528,0.063720,0.049705, #gamma=-0.6
                       0.669005,0.484214,0.402954,0.066843,0.058063,0.044907),#gamma=-0.7
            dim=c(6,num));
  
  if(eta==1.0)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q1[j,i];
  }
  
  else if(eta==0.5)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q05[j,i];
    
  }
  else if(eta==2.0)
  {
    for(i in 1:num)
      for(j in 1:6)
        q[i,j]=q2[j,i];
  }
  else
  {
    cat("No quantile table for eta=",eta," !\n");
    cat("eta must be among {0.5, 1, 2} !!\n");
    return;
  }
  
  
  for(i in 1:num)
  {
    if(i==1 && gamma>= vr[i])
    {
      for (j in 1:6)
        qgamma[j]=q[i,j];
      i=num+1;
    }
    else if(i>1 && i< num && gamma< vr[i-1] && gamma>= vr[i])
    {
      
      for (j in 1:6)
        qgamma[j]=q[i,j]+(gamma-vr[i])/(vr[i-1]-vr[i])*(q[i-1,j]-q[i,j]);
      i=num+1;
    }
    else if(i== num && gamma < vr[i])
    {
      for (j in 1:6)
        qgamma[j]=q[i,j];
      i=num+1;
    }
  }
  return(qgamma);
} # end this function

########################################################################################
##                                                                                    ##
##                                                                                    ##
##  Do not chanage the codes above if you are not very familar with it !              ##
##                                                                                    ##
##                                                                                    ##
########################################################################################

















########################################################################################
##                                                                                    ##
##                                                                                    ##
##  You can modify the main functions as follows according to your requirements.      ##
##                                                                                    ##
##                                                                                    ##
########################################################################################

# The default input function to read the data file such as ABNARMO.txt
readdata=function(filename="", fig="plot")
{
  path=getwd();
  X=read.table(paste(path,"\\",filename,sep=""),dec=".");
  n0=length(X);
  X=X[!is.na(X)];
  n1=length(X);
  if(fig=="plot")
  {
    #  windows();
    plot(X,main=filename,ylab="data",xlab=paste("available sample size n=", as.character(n1),sep=""));
  }
  
  if(n0-n1>0)
  {
    cat("the total sample size is:", n0, "\n");
    cat("the number of missing values is:", n0-n1, "\n");
    cat("the available sample size is:", n1,"\n");
  }
  return(X);      
} # end of the function readdata()



# Calculate the maximum likelihood (ML) estimators for EVI and its p-confidence interval for a sequence of k. 
MLgamma=function(X,k,p=0.95,fig="plot")
{
  # Here, ks could be a vector.
  # p is the confidence level. It must be set to be 0.0, 0.90, 0.95, 0.99.
  
  X=X[!is.na(X)];
  n=length(X);
  X=sort(X);
  
  # To make sure that k is not too large or too small.
  if(max(k)>0.80*n){cat("Some of your k's are too large. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  if(min(k)<5)     {cat("Some of your k's are too small. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  
  sizek=length(k);
  origin_rn=numeric(sizek); # the (original) ML-estimator of the extreme value index. We set gamma=-0.99 if we can not find the root.
  rn=numeric(sizek);  # the new ML-estimator, i.e. gamma=max(gamma,-0.499) 
  an=numeric(sizek);  # the ML-estimator for the scale
  bn=numeric(sizek);  # the ML-estimator for the shift
  
  theta=MLGPDtheta(X,n,k);# theta:=-gamma/sigma
  
  for(i in 1:sizek)
  {            
    if(theta[i] != 0.99/(X[n]-X[n-k[i]]))
    {
      rn[i]=cgamma(X,n,k[i],theta[i]);
      an[i]=-rn[i]/theta[i];
    }
    else # no ML estimation for theta.
    {
      rn[i]= -0.99;
      an[i]=X[n]-X[n-k[i]];
    }
    bn[i]=X[n-k[i]];
    
    origin_rn[i]=rn[i]; # records the original ML-estimator ( > -1.0).
    if(rn[i]<= -0.499)  # Note that the asymptotic normality of ML-estimator holds only for gamma>-0.5. Here we set gamma=max(gamma,-0.499).
    {  rn[i]=-0.499;
    an[i]=X[n]-X[n-k[i]];
    }
  }
  
  cat("############################################################################### \n");
  cat("##                                                                           ## \n");
  cat("##  Note that for ML-estimation we set gamma=max(gamma,-0.499) since the     ## \n");
  cat("##       asymptotic normality holds only for gamma>-0.5!                     ## \n");
  cat("##                                                                           ## \n");
  cat("############################################################################### \n\n");
  
  if(p==0.90 | p==0.95 | p==0.99) # We need calculate the p-condidence interval for the ML-estimator.
  {
    lowuprn=ConfMLgamma(X,n,k,rn,p);
    if(sizek==1) # lowuprn is a sequence with length=2.
    {
      lowrn=lowuprn[1];
      uprn=lowuprn[2];
    }
    else # lowuprn is a marix with row=length(k), column=2.
    {
      lowrn=lowuprn[,1];
      uprn=lowuprn[,2];
    }
    if(fig=="plot" && sizek >1)
    {
      #  windows();
      allgamma=cbind(rn,lowrn,uprn);
      matplot(k,allgamma,type="l",main=paste("Maximum  Likelihood,  p=", as.character(p),sep=""),xlab="k",ylab=expression(gamma),col=c(1,2,2),sub=paste("available samplesize n=",as.character(n)));
      return(cbind(k,origin_rn,rn, an,bn,lowrn,uprn));
    }  
    else
    {
      if(sizek==1){
        cat("n=",n,"  k=",k,"\n\n");
        cat("The ML-estimator for the extreme value index is ",rn[1],",\n");
        cat("            and its ",p," cofidence interval is [",lowrn[1],",",uprn[1],"].\n");
        cat("    The original ML-estimator for the EVI is ",origin_rn[1],".\n");
        cat("The ML-estimator for the scale is ", an[1],".\n");
        cat("The ML-estimator for the shift is ", bn[1],".\n\n");
      } 
      return(cbind(k,origin_rn,rn,an,bn,lowrn,uprn));
    }        
  }
  else # p=0 or other real number, i.e. point estimator is enough.
  {
    if(fig=="plot" && sizek >1)
    {
      #  windows();
      matplot(k,rn,type="l",main="Maximum  Likelihood  Estimation",xlab="k",ylab=expression(gamma),sub=paste("available samplesize n=",as.character(n)));
      return(cbind(k,origin_rn,rn,an,bn));
    }  
    else
    {
      if(sizek==1){
        cat("n=",n,"  k=",k,"\n\n");
        cat("The ML-estimator for the extreme value index is ",rn[1],".\n");
        cat("    The original ML-estimator for the EVI is ",origin_rn[1],".\n");
        cat("The ML-estimator for the scale is ", an[1],".\n");
        cat("The ML-estimator for the shift is ", bn[1],".\n\n");
      }
      return(cbind(k,origin_rn,rn,an,bn));
    }        
  }
} # end of the function MLgamma()



# Calculate the test statistics and the 0.95 quantiles of the limiting random variable
# by maximum likelihood (ML) estimation for a sequence of k, and present a plot.
# For more details see Drees, de Haan and Li (2006).
MLTestEVC1d=function(X,k,eta=1.0)
{
  # Here, k could be a vector or an integer.
  # eta must be set to be 0.5, 1.0, or 2.0. 
  # The defult value is set t be 1.0, which is the bset eta (see Huesler and Li (2006)) 
  X=X[!is.na(X)];
  n=length(X);
  X=sort(X);
  
  # To make sure that k is not too large or too small.
  if(max(k)>0.80*n){cat("Some of your k's are too large. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  if(min(k)<5)     {cat("Some of your k's are too small. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  
  sizek=length(k);
  
  parRAB=MLgamma(X,k,0.0);
  origin_rn=parRAB[,2];
  rn=parRAB[,3];
  an=parRAB[,4];
  bn=parRAB[,5];
  
  q95=numeric(sizek);
  teststat=numeric(sizek);
  for(i in 1:sizek)
  {
    teststat[i]=MLTest(X,n,k[i],rn[i],an[i],bn[i],eta);
    temp=MLQuanOfLimRV(rn[i],eta);
    # Here temp is a vector, including the 0.99,0.975,0.95,0.05,0.025 and 0.01 -quantiles of the limiting random variable
    # We only use the 0.95-quantile.
    q95[i]=temp[3];
  }
  
  cplots=cbind(teststat,q95);
  if(sizek>1)
  {
    #  windows();
    matplot(k,cbind(teststat,q95),main=paste("Drees, de Haan and Li (2006)'s method, eta=", as.character(eta),sep=""),ylab="",sub=paste("available sample size n=", as.character(n),sep=""),type="l",col=c(1,2));
    yp=par("yaxp");yl=yp[2];xl=0.75*max(k);
    legend(xl,yl,legend=c("test statistic", "0.95 quantile"),col=c(1,2),lty=1:2);
  }
  else 
  {
    cat("Drees, de Haan and Li (2006)'s test methodology (ML) with eta=",eta,":\n");
    cat("     the value of the test statistic is:        ",teststat[1],",\n");
    cat("     the 0.95 quantile of the limiting r.v. is: ",q95[1], ".\n\n");
  }
  return(cbind(k,origin_rn,rn,an,bn,teststat,q95));
} # end of the fnction MLTestEVC1d()



#Calculate the moment (M) estimators and its p-confidence interval (if you want) for EVI for a sequence of k. 
Mgamma=function(X,k,p=0.95,fig="plot")
{
  # Here, k could be a vector or an integer.
  # p is the confidence level. It must be set to be 0.0, 0.90, 0.95, 0.99.
  X=X[!is.na(X)];
  n=length(X);
  X=sort(X);
  
  # To make sure that k is not too large or too small.
  if(max(k)>0.80*n){cat("Some of your k's are too large. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  if(min(k)<5)     {cat("Some of your k's are too small. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  
  sizek=length(k);
  rnP=numeric(sizek); # the moment estimator of the positive part of EVI
  rnN=numeric(sizek); # the moment estimator of the begative part of EVI 
  rn=numeric(sizek);  # the moment estimator of the EVI
  an=numeric(sizek);  # the moment estimator for the scale
  bn=numeric(sizek);  # the moment estimator for the shift
  
  for(i in 1:sizek)
  {
    rnP[i]=gammaP(X,n,k[i]);
    rnN[i]=gammaN(X,n,k[i],rnP[i]);
    rn[i]=rnP[i]+rnN[i];
    if(rn[i]>=0)  {an[i]=X[n-k[i]]*rnP[i];}
    else          {an[i]=(1-rn[i])*X[n-k[i]]*rnP[i];}
    bn[i]=X[n-k[i]];
  }
  
  if(p==0.90 | p==0.95 | p==0.99) # We need calculate the p-condidence interval for the ML-estimator.
  {
    lowuprn=ConfMgamma(X,n,k,rn,p);
    if(sizek==1) # lowuprn is a sequence with length=2.
    {
      lowrn=lowuprn[1];
      uprn=lowuprn[2];
    }
    else # lowuprn is a marix with row=length(k), column=2.
    {
      lowrn=lowuprn[,1];
      uprn=lowuprn[,2];
    }
    if(fig=="plot" && sizek >1)
    {
      # windows();
      allgamma=cbind(rn,lowrn,uprn);
      matplot(k,allgamma,type="l",main=paste("Monment  Estimation,  p=", as.character(p),sep=""),xlab="k",ylab=expression(gamma),col=c(1,2,2),sub=paste("available samplesize n=",as.character(n)));
      return(cbind(k,rnP,rnN,rn, an,bn,lowrn,uprn));
    }  
    else
    {
      if(sizek==1){
        cat("n=",n,"  k=",k,"\n\n");
        cat("The moment estimator for the extreme value index (EVI) is ",rn[1],",\n");
        cat("            and its ",p," cofidence interval is [",lowrn[1],",",uprn[1],"].\n");
        cat("    The moment estimators for the positive and negative EVI are ",rnP[1],"and", rnN[1], "respectively.\n");
        cat("The moment estimator for the scale is ", an[1],".\n");
        cat("The moment estimator for the shift is ", bn[1],".\n\n");
      } 
      return(cbind(k,rnP,rnN,rn,an,bn,lowrn,uprn));
    }        
  }
  else # p=0 or other real number, i.e. point estimator is enough.
  {
    if(fig=="plot" && sizek >1)
    {
      # windows();
      matplot(k,rn,type="l",main="Monment  Estimation",xlab="k",ylab=expression(gamma),sub=paste("available samplesize n=",as.character(n)));
      return(cbind(k,rnP,rnN,rn,an,bn));
    }  
    else
    {
      if(sizek==1){
        cat("n=",n,"  k=",k,"\n\n");
        cat("The moment estimator for the extreme value index (EVI) is ",rn[1],".\n");
        cat("    The M-estimators for the positive and negative EVI are ",rnP[1],"and", rnN[1], "respectively.\n");
        cat("The moment estimator for the scale is ", an[1],".\n");
        cat("The moment estimator for the shift is ", bn[1],".\n\n");
      }
      return(cbind(k,rnP,rnN,rn,an,bn));
    }        
  }    
} # end of the function Mgamma()



# Calculate the test statistics and the 0.95 quantiles of the limiting random variable
# by moment estimation (M) for a sequence of k, and present a plot.
# For more details see Dietrich, de Haan and Huesler (2002).
MTestEVC1d=function(X,k,eta=2.0)
{
  # Here, k could be a vector or an integer.
  # eta must be set to be 0.5, 1.0, or 2.0. 
  # The defult value is set t be 2.0, which is the bset eta (see Huesler and Li (2006)) 
  X=X[!is.na(X)];
  n=length(X);
  X=sort(X);
  
  # To make sure that k is not too large or too small.
  if(max(k)>0.80*n){cat("Some of your k's are too large. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  if(min(k)<5)     {cat("Some of your k's are too small. Please reset your k !\n The value of k must be between 5 and ",floor(0.8*n),".\n");return(); }
  
  sizek=length(k);
  
  rnPN=Mgamma(X,k,0.0);
  rnP=rnPN[,2];
  rnN=rnPN[,3];
  rn=rnPN[,4];
  an=rnPN[,5]
  bn=rnPN[,6]
  
  q95=numeric(sizek);
  teststat=numeric(sizek);
  for(i in 1:sizek)
  {
    teststat[i]=Mtest(X,n,k[i],rnP[i],rnN[i],eta);
    ttemp=MQuanOfLimRV(rn[i],eta);
    # Here temp is a vector, including the 0.99,0.975,0.95,0.05,0.025 and 0.01 -quantiles of the limiting random variable
    # We only use the 0.95-quantile.
    q95[i]=ttemp[3];
  }
  
  cplots=cbind(teststat,q95);
  if(sizek>1)
  {
    # windows();
    matplot(k,cbind(teststat,q95),main=paste("Dietrich, de Haan and Huesler (2002)'s method,  eta=", as.character(eta),sep=""),ylab="",sub=paste("available sample size n=", as.character(n),sep=""),type="l",col=c(1,2));
    yp=par("yaxp");yl=yp[2];xl=0.75*max(k);
    legend(xl,yl,legend=c("test statistic", "0.95 quantile"),col=c(1,2),lty=1:2);
  }
  else 
  {
    cat("Dietrich, de Haan and Huesler (2002)'s test methodology (M) with eta=",eta,":\n");
    cat("     the value of the test statistic is:        ",teststat[1],",\n");
    cat("     the 0.95 quantile of the limiting r.v. is: ",q95[1], ".\n\n");
  }
  return(cbind(k,rn,an,bn,teststat,q95));
} # end of the function MTestEVC1d()