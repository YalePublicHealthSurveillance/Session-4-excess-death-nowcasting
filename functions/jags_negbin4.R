library(rjags)
model_string_negbin4<-
  "model
{
  # Likelihood
  for( t in 1:n.dates ){
    for(d in 1:(D+1)){
  
    #First likelihood is for full data
    n[t,d] ~ dnegbin(p1[t,d],r1)
    # Conversions
    p1[t,d] <- r1/(r1+lambda[t,d])
  
    log(lambda[t,d]) <- ( alpha[t] +
            beta.logged2[d]*step(D-d) +
            sum.beta.logged2[t]*(1-step(D-d)) 
                    ) 
    }
   
    sum.lambda[t] <- sum(lambda[t,1:D])
    
    #If D=1 is not observed, use column D+1 and (N.first.obs+1):D first obs is in col D+1
    sum.n[t]  <- (1-step(N.first.obs[t]-1.5))*sum(n[t,(1:D)]) + 
            step(N.first.obs[t]-1.5)*(n[t,(D+1)] +sum(n[t,(N.first.obs[t]+1):D]))
  }

   alpha[1] ~ dnorm(0, 0.001)
  for(t in 2:n.dates){
     alpha[t] ~ dnorm((alpha[t-1]), tau2.alpha)
   }

  # Prior for neg binomial rate
  r1 ~ dgamma(0.001,0.001)

  ## Prior for beta
  beta.logged <- log(beta)
  beta.logged2 <- c(beta.logged,0)
  beta ~ ddirch(beta.priors)
  
  for( t in 1:n.dates ){

    sum.beta[t] <- sum(beta[1:(N.first.obs[t]) ])

    sum.beta.logged2[t] <- log(sum.beta[t])
  }

  # Prior for variance
  tau2.alpha ~ dgamma(alphat.shape.prior,alphat.rate.prior)

}
"


##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')

beta.priors <- rep(0.1, times=(ncol(reporting.triangle)))

##############################################
#Model Organization
##############################################
model_spec<-textConnection(model_string_negbin4)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('n.dates' =
                                   nrow(reporting.triangle),
                                 'n' = as.matrix(reporting.triangle[,-1]),
                                 'D' = ncol(reporting.triangle[,-1])-1,
                                 
                                 
                                 alphat.shape.prior=0.001,
                                 alphat.rate.prior=0.001,
                                 'N.first.obs'=(first.measured.date$first.week.measured-1),
                                 'beta.priors'=beta.priors
                                 
                       ),
                       n.adapt=5000, 
                       n.chains=3)

params<-c('sum.n','sum.lambda',
          'beta.logged', 'alpha','sum.beta')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=5000)
posterior_samples.all<-do.call(rbind,posterior_samples)
#post1.summary<-summary(posterior_samples)
#post_means<-colMeans(posterior_samples.all)
