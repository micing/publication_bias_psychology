#library("stats")
library("pwr")
library("RCurl")
source("functions.R")
load("publication_bias_data.RDta")

#### helper functions ####

minrange <- function(t1, t2, range, est, d=data$range){
  min(d[[range]][[est]]$min[d$theta == t1],
      d[[range]][[est]]$min[d$theta == t2])
}

maxrange <- function(t1, t2, range, est, d=data$range){
  max(d[[range]][[est]]$max[d$theta == t1],
      d[[range]][[est]]$max[d$theta == t2])
}

minrep <- function(t1, t2, rep, est, d=data$rep){
  rep = paste0("R",round(rep, 2))
  min(d[[rep]]$s2i[[est]][d[[rep]]$s2i$theta == t1],
      d[[rep]]$s2i[[est]][d[[rep]]$s2i$theta == t2])
}

maxrep <- function(t1, t2, rep, est, d=data$rep){
  rep = paste0("R", round(rep, 2))
  max(d[[rep]]$s2i[[est]][d[[rep]]$s2i$theta == t1],
      d[[rep]]$s2i[[est]][d[[rep]]$s2i$theta == t2])
}

#### test of publication bias ####

binom.test(35, 97, .90)


#### RPP data ####

rppurl=getURL("https://raw.githubusercontent.com/CenterForOpenScience/rpp/master/data_allformats/RPPdataConverted.csv")
rpp=read.csv(text = rppurl)
d=subset(rpp, !is.na(rpp$Replicate.R), select=c("N.O", "N.R", "Replicate.R"))
d$larger=d$N.R>d$N.O
d$smaller=d$N.R<d$N.O
d$same=d$N.R==d$N.O

sum(d$larger)
sum(d$same)
sum(d$smaller)

# Difference in power between original and replication

df_o = 54
df_r = 68
r = seq(.2,.4,.01)

delta_power =  pwr.r.test(n=df_r+2, r=r)$power - pwr.r.test(n=df_o+2, r=r)$power

max(delta_power)
min(delta_power)


#### Figure 2: main results ####

# outer limits of power: NAs indicate invalid (better than perfect) power.
data.frame(data$range$theta, data$range$outer$power$min, data$range$outer$power$max)[1:10,]
# likely limits of power
data.frame(data$range$theta, data$range$likely$power$min, data$range$likely$power$max)[1:15,]
# extrem to zero variance limits of power
data.frame(data$range$theta, data$range$alt2$power$min, data$range$alt2$power$max)[1:15,]

minrange(.05,.05, "outer", "power")

minrange(.05,.05, "likely", "power")
maxrange(.05,.05, "likely", "power")
minrange(.05,.2, "likely", "power")
maxrange(.05,.2, "likely", "power")
minrange(.05,.2, "alt", "power") # likely range with alternative variance estimates (smaller and larger)
maxrange(.05,.2, "alt", "power")
minrange(.05,.2, "alt2", "power") # likely range with zero to extreme variance
maxrange(.05,.2, "alt2", "power")


minrange(.1,.1, "likely", "posterior")
maxrange(.1,.1, "likely", "posterior")
minrange(.1,.1, "likely", "power")
maxrange(.1,.1, "likely", "power")

# at theta = .1: approximately 52% posterior and 67% power in replications 
# = 36% reproducibility (59% power in original studies and 1.2% type-1 errors):

.67*.52+.025*(1-.52)

minrange(.05,.2, "likely", "posterior")
maxrange(.05,.2, "likely", "posterior")
minrange(.05,.2, "alt", "posterior")
maxrange(.05,.2, "alt", "posterior")
minrange(.05,.2, "alt2", "posterior")
maxrange(.05,.2, "alt2", "posterior")

minrange(.05,.2, "likely", "positive")
maxrange(.05,.2, "likely", "positive")
minrange(.05,.2, "alt", "positive")
maxrange(.05,.2, "alt", "positive")
minrange(.05,.2, "alt2", "positive")
maxrange(.05,.2, "alt2", "positive")

minrange(.05,.2, "likely", "bias")
maxrange(.05,.2, "likely", "bias")
minrange(.05,.2, "alt", "bias")
maxrange(.05,.2, "alt", "bias")
minrange(.05,.2, "alt2", "bias")
maxrange(.05,.2, "alt2", "bias")
minrange(.05,.2, "outer", "bias")
maxrange(.05,.2, "outer", "bias")

minrange(.05,.05, "outer", "bias")
minrange(.2,.2, "outer", "bias")

minrange(.05,.05, "outer", "bias")
minrange(.975,.975, "outer", "bias")

# Figure 3: assuming different reproducibility rates

minrep(.05,.20, .46, "positive")
maxrep(.05,.20, .46, "positive")

minrep(.05,.20, .46, "bias")
maxrep(.05,.20, .46, "bias")

#### Discussion ####

# Johnsson et al
minrange(.07,.07, "likely", "power")
maxrange(.07,.07, "likely", "power")
minrange(.07,.07, "likely", "posterior")
maxrange(.07,.07, "likely", "posterior")
minrange(.07,.07, "likely", "positive")
maxrange(.07,.07, "likely", "positive")
minrange(.07,.07, "likely", "bias")
maxrange(.07,.07, "likely", "bias")
