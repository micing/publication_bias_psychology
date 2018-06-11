# NB:   "beta" refers to the type-2 error rate and is the symbol used in equations. 
#       "Beta" refers to the Beta distribution that is used to model variance in power (1-beta).

# find the second shape parameter to achieve the specified mean of a Beta distribution
findBetaPar <- function(m, shape) { 
  if (m==.5) {return(c(shape, shape))}
  # m=s1/(s1+s2); s1=(-s2*m)/(m-1); s2=s1*(1/m-1)
  if (m>.5) {return(c((-shape*m)/(m-1), shape)) }
  if (m<.5) {return(c(shape, shape*(1/m-1))) }
}

# Beta distribution parameterized by a mean and a single shape parameter 
dBetam <- function(x, m=.475, shape=1, mmin=.001, lower=0, upper=.95, reportValidMeanRange=FALSE) {
  if (reportValidMeanRange) {return(c(lower+mmin,upper-mmin))}
  x=x/(upper-lower)+lower
  m=m/(upper-lower)+lower
  par=findBetaPar(m, shape)
  dbeta(x, par[1], par[2])/(upper-lower)
}

# bimodal Beta distribution
dbimodal <- function(x, m=.5, shape=1, locations=c(.1,.9), lower=0, upper=.95, reportValidMeanRange=FALSE) {
  if (reportValidMeanRange) {return(locations*(upper-lower)+lower)}
  a=locations[1]
  b=locations[2]
  x=x/(upper-lower)+lower
  m=m/(upper-lower)+lower
  w <- function() (m-b)/(a-b) # m = ax + b(1-x) = (m-b)/(a-b)
  (dBetam(x=x, m=a, shape=shape, lower=0, upper=1)*w() +
   dBetam(x=x, m=b, shape=shape, lower=0, upper=1)*(1-w()))/(upper-lower)+lower 
}

# prespecified bimodal distributions
dbimodal1090 = function(x, m, shape, lower=0, upper=.95, reportValidMeanRange=FALSE) 
{dbimodal(x, m=m, locations=c(.1, .90), shape=shape, lower=lower, upper=upper, 
            reportValidMeanRange=reportValidMeanRange)}
dbimodal0595 = function(x, m, shape, lower=0, upper=.95, reportValidMeanRange=FALSE) 
{dbimodal(x, m=m, locations=c(.05, .95), shape=shape, lower=lower, upper=upper, 
            reportValidMeanRange=reportValidMeanRange)}

# variance of a probability density function
dvar <- function(density, m, shape){
  fx <- function(x)  {density(x, m=m, shape=shape)*x}
  fx2 <- function(x) {density(x, m=m, shape=shape)*x^2}
  integrate(fx2, 0, 1)$value - integrate(fx, 0, 1)$value^2
}

# mean of a probability density function
dmean <- function(density, m, shape){
  fx <- function(x)  {density(x, m=m, shape=shape)*x}
  integrate(fx, 0, 1)$value
}

# Equation 5 & 8: posterior probability
theta_hat <- function(beta, theta, alpha=.05) {theta*(1-beta) / (theta*(1-beta) + alpha*(1-theta))}
Etheta_hat <- function(m, theta, alpha_o=.05, alpha_r=.025, density=dBetam, shape=1) {
  f <- function(beta) {density(beta, m=m, shape=shape) * theta_hat(beta, alpha=alpha_o, theta=theta)}
  integrate(f, 0, 1)$value
}

# Equation 6 & 9: reporducibility
R <- function(beta_o, delta, theta, alpha_o=.05, alpha_r=.025) {
  if (is.na(delta)) {beta_r=0 } # this means power = 100% in replication studies
  else { beta_r = beta_o + delta }
  theta_hat(beta=beta_o, theta=theta)*(1-beta_r)+alpha_r*(1-theta_hat(beta=beta_o, theta=theta))
  }

ER <- function(beta_o, delta, theta, alpha_o=.05, alpha_r=.025, density=dBetam, shape=1) {
  if (is.na(delta)) {beta_r=0 } # this means power = 100% in replication studies
  else { beta_r = beta_o + delta }
  Eth=Etheta_hat(m=beta_o, theta=theta, density=density, shape=shape)
  Eth*(1-beta_r)+alpha_r*(1-Eth)
}

# Equation 3
positiveEvidence <- function(beta, theta, alpha=.05) {theta*(1-beta)+alpha*(1-theta)}

# odds that observed negative evidence is supressed from publication  
publicationBias <- function(pos, obs_pos=.90){((1-pos)/pos)/((1-obs_pos)/obs_pos)}

# solution to equation 7 solved analytically 
# condition describe four different assumptions of power in replication studies (see main text)
beta <- function(theta, condition=1){ 
  switch(condition, 
         {1/200*(164 - sqrt(670/theta + 626))},      ## lower bound; power = same
         {(2627*theta - 67)/(2560*theta)},           ## upper bound; power = 100%
         {1/200*(158 - sqrt(670/theta + 230))+3/50}, ## lower likely; power = +6%
         {1/200*(154 - sqrt(670/theta + 6))+1/10})   ## upper likely; power = +10% 
}

# this functions returns the delta in beta between original 
# and replication studies for the four different conditions (see beta above)
# and a mid point estimate (condition 5)
betaDelta <- function(condition) switch(condition, 0, NA, -.06, -.1, -.08)

power <- function(theta, condition=1){
  1-beta(theta, condition)
}

# solution to equation 8 and 9
Epower <- function(theta, alpha_o=.05, alpha_r=.025, R=35/97, delta=0, density=dBetam, shape=1) {
  ERdiff <- function(beta_o) {
    abs(R-ER(beta_o=beta_o, delta=delta, theta=theta, alpha_o=alpha_o, alpha_r=alpha_r, density=density, shape=shape))
  }
  validMeanRange = density(NA, reportValidMeanRange=TRUE) # get the valid search range for the optimizer
  1-optimize(ERdiff, lower=validMeanRange[1], upper=validMeanRange[2])$minimum
} 

#### functions to generate, clean and organize data ####
generateData <- function(theta, condition=1, shape=1, density=dBetam, tol=.0001, R=35/97, alpha_r=.025) {
  
  delta=betaDelta(condition)
  d=data.frame(theta=theta, power=NA,  posterior=NA, positive=NA, bias=NA, R=NA, converged=NA, validPower=NA)

  for (i in 1:length(theta)) { # loop to catch errors (rare)
    if (!is.na(shape)) { # estimate expected values (equation 10)
      d[i, "power"] = tryCatch({Epower(theta=d[i, "theta"], shape=shape, density=density, delta=delta, R=R)}, error=function(e) NA)
      d[i, "posterior"] = tryCatch({Etheta_hat(m=1-d[i, "power"], theta=d[i, "theta"], shape=shape, density=density)}, error=function(e) NA)
      d[i, "R"] = tryCatch({ER(beta_o=1-d[i, "power"], theta=d[i,"theta"], shape=shape, density=density, delta=delta)}, error=function(e) NA)
    }
    else{ # naive solution (equation 7)
      d[i, "power"] = power(d[i, "theta"], condition=condition)
      d[i, "posterior"] = theta_hat(beta=1-d[i, "power"], theta=d[i, "theta"])
      d[i, "R"] = R(1-d[i, "power"], delta=delta, theta=d[i,"theta"])
    }
  }
  d$positive = positiveEvidence(beta=1-d$power, theta=d$theta)
  d$bias = publicationBias(d$positive)
  
  # double checking the math by calculating reproducibility (R_b) from the estimated posterior
  if (is.na(delta)) { d$R_b = d$posterior + alpha_r*(1-d$posterior)}
  else { d$R_b = (d$power-delta)*d$posterior + alpha_r*(1-d$posterior) }
  
  #indicate valid estimates
  d$converged = abs(d$R - switch(as.numeric(is.na(shape))+1, R, .36 )) < tol
  d$validPower = d$power - switch(as.numeric(is.na(delta))+1, delta, 0 )  < 1
  d$congruentR = abs(d$R - d$R_b) < tol
  return(d)
}

cleanData <- function(data){
  d=list()
  for (l1 in names(data)) {
    for (l2 in names(data[[l1]])) {
      t=data[[l1]][[l2]]
      t[(is.na(t$converged) | !t$converged | !t$validPower | !t$congruentR) , 
        c("power", "posterior", "positive", "bias")] <- NA 
      d[[l1]][[l2]]=t
      }
    }
  return(d)
}

mmax <- function(data){
  d=data[[1]]
  for (i in 2:length(data)) {
    d=pmax(d, data[[i]], na.rm=TRUE)
  }
  d
}

mmin <- function(data){
  d=data[[1]]
  for (i in 2:length(data)) {
    d=pmin(d, data[[i]], na.rm=TRUE)
  }
  d
}

generateRanges <- function(data, likely="s2i", small="s1", large="s1b1090" , extreme="s1b0595", zero="zero", 
                           est=c("power", "posterior", "positive", "bias")) {
  d=list()
  for (e in est) {
    d$likely[[e]]$max = mmax(list(data$c3[[likely]][[e]],data$c4[[likely]][[e]]))
    d$likely[[e]]$min = mmin(list(data$c3[[likely]][[e]],data$c4[[likely]][[e]]))
    
    d$alt[[e]]$max = mmax(list(data$c3[[small]][[e]],data$c4[[small]][[e]], 
                               data$c3[[large]][[e]],data$c4[[large]][[e]]))
    d$alt[[e]]$min = mmin(list(data$c3[[small]][[e]],data$c4[[small]][[e]], 
                               data$c3[[large]][[e]],data$c4[[large]][[e]]))
    d$alt2[[e]]$max = mmax(list(data$c3[[zero]][[e]],data$c4[[zero]][[e]], 
                                 data$c3[[extreme]][[e]],data$c4[[extreme]][[e]]))
    d$alt2[[e]]$min = mmin(list(data$c3[[zero]][[e]],data$c4[[zero]][[e]], 
                                 data$c3[[extreme]][[e]],data$c4[[extreme]][[e]]))

    d$zero[[e]]$c3 = data$c3[[zero]][[e]] # used to determine direction of bias vs extreme in figure 2
    d$zero[[e]]$max = mmax(list(data$c3[[zero]][[e]],data$c4[[zero]][[e]]))
    d$zero[[e]]$min = mmin(list(data$c3[[zero]][[e]],data$c4[[zero]][[e]]))
    
    d$small[[e]]$max = mmax(list(data$c3[[small]][[e]],data$c4[[small]][[e]]))
    d$small[[e]]$min = mmin(list(data$c3[[small]][[e]],data$c4[[small]][[e]]))
    
    d$large[[e]]$max = mmax(list(data$c3[[large]][[e]],data$c4[[large]][[e]]))
    d$large[[e]]$min = mmin(list(data$c3[[large]][[e]],data$c4[[large]][[e]]))
    
    d$extreme[[e]]$c3 = data$c3[[extreme]][[e]] # see above
    d$extreme[[e]]$max = mmax(list(data$c3[[extreme]][[e]],data$c4[[extreme]][[e]]))
    d$extreme[[e]]$min = mmin(list(data$c3[[extreme]][[e]],data$c4[[extreme]][[e]]))
    
    d$outer[[e]]$max = mmax(list(data$c1[[zero]][[e]],data$c2[[zero]][[e]], 
                                 data$c1[[extreme]][[e]],data$c2[[extreme]][[e]]))
    d$outer[[e]]$min = mmin(list(data$c1[[zero]][[e]],data$c2[[zero]][[e]], 
                                 data$c1[[extreme]][[e]],data$c2[[extreme]][[e]]))
  }
  d
}
