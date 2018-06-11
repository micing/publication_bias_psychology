source("functions.R")

#### generate data ####
n=50
reserved = c(seq(.025, .026, .0001), seq(.025, .03, .001), seq(.05,.95,.05), .07, 1/3, 1/6, .975)
sequence=exp(seq(log(.025), log(.975), (log(.975)-log(.025))/n))
theta=sort(unique(c(reserved, sequence)))

### main datasets ####

data=list()

# condition refer to: 1=same power in replication, 2=100% power in replications, 3=+6%, 4=+10%
condition = c(1,2,3,4) 

for (c in condition) {
  data$mainRaw[[paste0("c", c)]]$zero=generateData(theta, condition=c, shape=NA, density=NA)
  data$mainRaw[[paste0("c", c)]]$s50=generateData(theta, condition=c, shape=50, density=dBetam)
  data$mainRaw[[paste0("c", c)]]$s1=generateData(theta, condition=c, shape=1, density=dBetam)
  data$mainRaw[[paste0("c", c)]]$s2i=generateData(theta, condition=c, shape=1/2, density=dBetam)
  data$mainRaw[[paste0("c", c)]]$s3i=generateData(theta, condition=c, shape=1/3, density=dBetam)
  data$mainRaw[[paste0("c", c)]]$s1b1090=generateData(theta, condition=c, shape=1, density=dbimodal1090)
  data$mainRaw[[paste0("c", c)]]$s2b1090=generateData(theta, condition=c, shape=2, density=dbimodal1090)
  data$mainRaw[[paste0("c", c)]]$s1b0595=generateData(theta, condition=c, shape=1, density=dbimodal0595)
}

data$main=cleanData(data$mainRaw)
data$main$theta=theta

#### dataset describing ranges of estimates ####

## assumed variances
small="s1"
likely="s2i"
large="s1b1090"
extreme="s1b0595"
zero= "zero"

data$range = generateRanges(data$main, likely=likely, small=small, large=large, extreme=extreme, zero=zero)
data$range$theta=theta

#### dataset with different assumed true reproducibilites ####

f<-function(c) c(binom.test(35, 97,0, conf.level=c)$conf.int[1], binom.test(35, 97,0, conf.level=c)$conf.int[2])
fv = Vectorize(f)

CIs = c(.95, .75, .50)
Rs=sort(c(35/97, fv(CIs)))

for (R in Rs) {
  data$repRaw[[paste0("R", signif(R, 2))]]$s2i=generateData(theta, condition=5, shape=1/2, density=dBetam, R=R)
}

data$rep=cleanData(data$repRaw)
data$rep$theta=theta

save(data, file="publication_bias_data.RDta")

#### report inconsistencies ####
summary=data.frame()
for (l1 in names(data$mainRaw)) {
  for (l2 in names(data$mainRaw[[l1]])) {
      d=data$mainRaw[[l1]][[l2]]
      summary=rbind(summary,data.frame(l1=l1, l2=l2,
              missing = length(d[is.na(d$converged), 1]), 
              not_converged = length(d[!d$converged & !is.na(d$converged), 1]),
              not_validPower = length(d[!d$validPower & !is.na(d$converged), 1]),
              not_congruentR = length(d[!d$congruentR & !is.na(d$converged), 1])
              ))
  }
}

summary


