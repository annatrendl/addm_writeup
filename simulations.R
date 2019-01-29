rm(list = ls())
setwd("C:/Anna/exp2")
library(data.table)
library(ggplot2)
library(boot)

source("DDM_sim.R")

#set up functions
#first generate data
get.sim <- function(item1_r, item2_r, item3_r, noise, speed.of.int, no.sim) {
data <- data.table(t(replicate(no.sim,DDM_simulation(item1 = item1_r, item2 = item2_r, item3 = item3_r, sigma = noise, speed.of.integration=speed.of.int))))
setnames(data, old= c("V1", "V2"), new = c("Chosen", "When"))
data[, Chosen := as.factor(Chosen)]
setkey(data,Chosen)
return(data)
}

#second bootstrap fun
bs_RT <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  statistics_RT <- d[,list(RT = as.vector(quantile(When,probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))),by = Chosen]
  return(statistics_RT[,RT])
} 

bs_CP <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  statistics_CP <- as.numeric(d[,table(Chosen)/nrow(d)])
  return(statistics_CP)
} 


#third confidence intervals
getCI <- function(bootobject, w) {
  b <- boot.ci(bootobject, index = w)
}

#generate data
#dataset <- get.sim(item1_r = 1/7, item2_r = 6/7, item3_r = 1, noise = 0.2, speed.of.int = 0.1, no.sim = 2000)

#plot it
#ggplot(dataset, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf()

#bootsrap
#boot_RT <- boot(data = dataset, statistic= bs_RT, R = 15000)


#boot_CP <- boot(data = dataset, statistic= bs_CP, R = 5000)


#function to create dataset with lower and upper cis
createdataset <- function(bootobject) {
  data <- NULL
  sapply(1:length(bootobject$t0), function (x) {
  CIs <- getCI(bootobject,x)
  CIs <- as.numeric(CIs$normal[1,2:3])
  data <<- data.table(rbind(data, t(CIs)))
  print(x)
  }
  )
  data[, value := bootobject$t0]
  setnames(data,old=c("V1", "V2"), new = c("lower", "upper"))
  return(data)
}

#RT_results <- createdataset(boot_RT)
#RT_results[, Chosen := as.factor(rep(c(1,2,3), each = 5))]
#RT_results[, Perc := rep(c(0.025, 0.25, 0.5, 0.75, 0.975), 3)]
#setnames(RT_results, old = c("value"), new = c("When"))

#CP_results <- createdataset(boot_CP)
#CP_results[, Chosen := as.factor(c(1,2,3))]
#setnames(CP_results, old = c("value"), new = c("Proportion"))


#ggplot(dataset, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf() + scale_x_continuous() + geom_errorbarh(data = RT_results, aes(y = Perc, x = When, xmin = lower, xmax = upper),height = 0.05)

#ggplot(CP_results, aes(x=Chosen, y=Proportion)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=lower, ymax=upper),width=.2) + ylim(c(0,1))





#Without bootstrapping
source("multiplot.R")
#generate data
generate.plots <- function(item1, item2, item3, noise, speed.of.int, no.sim) {

dataset <- get.sim(item1_r = item1, item2_r = item2, item3_r = item3, noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)

plot1 <- ggplot(dataset, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf()

plot2 <- ggplot(dataset, aes(x= Chosen)) + geom_bar()

print(multiplot(plot1, plot2))
}


