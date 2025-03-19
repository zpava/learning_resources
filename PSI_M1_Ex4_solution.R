#set an initial seed value to make results reproducible
set.seed(12345)   

##another big change
# the first part of the program defines the simulation 'cholc.sim'
# 1st argument Nstudies = number of studied to be simulated
# 2nd argument N = number of observations per study
# 3rd argument mu = mean of normal distribution
# 4th argument sigmasq = variance of normal distribution
# 5th argument k=proportion of observations contaminated
# 6th argument con.low=lower value for contaminated samples from uniform distribution 
# 7th argument con.high=upper value for contaminated samples from uniform distribution
  
cholc.sim=
  function(Nstudies,N,mu,sigmasq,k=0,con.low=0,con.upp=0) {
    out=list(means=0,meds=0,vars=0,IQRs=0)
    for (i in 1:Nstudies) {
      obsi=rnorm(N,mu,sqrt(sigmasq))
      if ((k>0)&(k<1)) {
        randi=runif(N)
        coni=runif(N,con.low,con.upp)
        obsi[randi<=k]=coni[randi<=k]
      }
      out$means[i]=mean(obsi)
      out$meds[i]=median(obsi)
      out$vars[i]=var(obsi)
      out$IQRs[i]=IQR(obsi)
      out$Rsq[i]=IQR(obsi)^2/1.34898^2
    }
    out}
#-------------------------------------------------------------------------
# Module 1 Exercise 4
# 1000 studies of size 100 
# From normal distribution with mean 3.5 and variance 0.4
# Contamination of 1% of observations from a U(0,10) distribution
#-------------------------------------------------------------------------
# k=0%
cholc0=cholc.sim(1000,100,3.5,0.4,k=0,0,10)
# mean and variance of variance estimator
mean(cholc0$vars)
var(cholc0$vars)
# mean and variance of R2 estimates
mean(cholc0$Rsq)
var(cholc0$Rsq)

# copy and edit previous code for k=0%, 1%, and 5%

##Bias
Bias_Sn1_0 = 0.4 - mean(cholc0$vars)
Bias_Sn1_0
Bias_R2_0 = 0.4 - mean(cholc0$Rsq)
Bias_R2_0

##Efficiency with variance

E_v_R2_0 = var(cholc0$vars)/var(cholc0$Rsq)
E_v_R2_0
E_v_sn_0 = var(cholc0$Rsq)/var(cholc0$vars)
E_v_sn_0
##MSE calculation

MSE_Sn1_0 = (Bias_Sn1_0)^2 + var(cholc0$vars)
MSE_Sn1_0
MSE_R2_0 = (Bias_R2_0)^2 + var(cholc0$Rsq)
MSE_R2_0
##Efficiency with MSE

E_M_R2_0 = MSE_Sn1_0/MSE_R2_0
E_M_R2_0
E_M_sn_0 = MSE_R2_0/MSE_Sn1_0
E_M_sn_0


# k=1%
cholc1=cholc.sim(1000,100,3.5,0.4,k=0.01,0,10)
# mean and variance of variance estimator
mean(cholc1$vars)
var(cholc1$vars)
# mean and variance of R2 estimates
mean(cholc1$Rsq)
var(cholc1$Rsq)

##Bias
Bias_Sn1_1 = 0.4 - mean(cholc1$vars)
Bias_Sn1_1
Bias_R2_1 = 0.4 - mean(cholc1$Rsq)
Bias_R2_1

##Efficiency with variance

E_v_R2_1 = var(cholc1$vars)/var(cholc1$Rsq)
E_v_R2_1
E_v_sn_1 = var(cholc1$Rsq)/var(cholc1$vars)
E_v_sn_1
##MSE calculation

MSE_Sn1_1 = (Bias_Sn1_1)^2 + var(cholc1$vars)
MSE_Sn1_1
MSE_R2_1 = (Bias_R2_1)^2 + var(cholc1$Rsq)
MSE_R2_1
##Efficiency with MSE

E_M_R2_1 = MSE_Sn1_1/MSE_R2_1
E_M_R2_1
E_M_sn_1 = MSE_R2_1/MSE_Sn1_1
E_M_sn_1

# k=5%
cholc5=cholc.sim(1000,100,3.5,0.4,k=0.05,0,10)
# mean and variance of variance estimator
mean(cholc5$vars)
var(cholc5$vars)
# mean and variance of R2 estimates
mean(cholc5$Rsq)
var(cholc5$Rsq)

##Bias
Bias_Sn1_5 = 0.4 - mean(cholc5$vars)
Bias_Sn1_5
Bias_R2_5 = 0.4 - mean(cholc5$Rsq)
Bias_R2_5

##Efficiency with variance

E_v_R2_5 = var(cholc5$vars)/var(cholc5$Rsq)
E_v_R2_5
E_v_sn_5 = var(cholc5$Rsq)/var(cholc5$vars)
E_v_sn_5
##MSE calculation

MSE_Sn1_5 = (Bias_Sn1_5^2) + var(cholc5$vars)
MSE_Sn1_5
MSE_R2_5 = (Bias_R2_5)^2 + var(cholc5$Rsq)
MSE_R2_5
##Efficiency with MSE

E_M_R2_5 = MSE_Sn1_5/MSE_R2_5
E_M_R2_5
E_M_sn_5 = MSE_R2_5/MSE_Sn1_5
E_M_sn_5

