setwd("C:/Anna/addm_writeup")
rm(list=ls())
library(ggplot2)
library(data.table)



aDDM <- function(r, d, theta.unattended, sigma) {
  # r is a vector with the values of the options
  # theta is the attentional discounting of the non attended options
  # d controls the speed of integration
  #theta.unattended <- 0.3
  #sigma <- 0
  # create a variable with the row index of the given value set within the subjective values matrix 
  #d <- 0.1

  #r <- c(7,4,5)
  set.seed(1)
  
  no.options <- length(r)
  I <- -(1:no.options)
  E <- c(0,0,0)
  chosen <- NA
  choice.time <- 0
  fixation.number <- 0
  fixations <- c(rep(1,2000),rep(2,3000),rep(3,2000))
  relative_values <- NULL
sapply(1:length(fixations), function (x) {
    fixated <- fixations[x]
    theta <- rep(theta.unattended, no.options)
    theta[fixated] <- 1
    # Vectorised version of a fixation
    noise <- matrix(rnorm(no.options, mean=0, sd=sigma), nrow=no.options)
    delta.E <- t(d * r * theta + noise)
    E <<- E + delta.E
    #print(E)
    V <- cbind(E[1,1] - pmax.int(E[1,2],E[1,3]), E[1,2] - pmax.int(E[1,1], E[1,3]), E[1,3] - pmax.int(E[1,1],E[1,2]))
    #print(V)
    relative_values <<- data.table(rbind(relative_values, V))
    #E <<- E[,1] # Save the last E for the next fixation
}
)
 return(relative_values)
}


data <- aDDM(r = c(4,3,6), d = 0.0002, theta.unattended = 0.3,sigma = 0.001)
data[V1 >= 1 | V2 >= 1 | V3 >= 1]
first <- min(data[V1 >= 1 | V2 >= 1 | V3 >= 1,which=TRUE])
data <- data[1:first,]

data[,time_step := .GRP, by = 1:nrow(data)]

setnames(data,old = c("V1", "V2", "V3"), new = c("left item", "center item", "right item"))


data_new <-  melt(data, id.vars = c("time_step"),
             measure.vars = c("left item", "center item", "right item"))
data_new[variable == "left item", variable := "Left Item"]
data_new[variable == "right item", variable := "Right Item"]
data_new[variable == "center item", variable := "Middle Item"]


ggplot(data = data_new, aes(x = time_step, y = value, colour = variable, group = variable)) + geom_line(size = 1.5) + ylim(c(-1.5,1.5)) +
  scale_colour_manual(breaks = unique(data_new[,variable]), values = c("darkorange2","mediumpurple4","firebrick2")) +
  geom_hline(yintercept = 1, colour = "black") + geom_vline(xintercept = 2000, linetype = "dashed") +
  geom_vline(xintercept = 5000, linetype = "dashed") + annotate("text", x = 700, y = 1.1, label = "Decision Threshold", colour = "black", size = 5) +
  annotate("text", x = 700, y = -1.4, label = "Left Item Fixated", colour = "darkorange2", size = 5) +
  annotate("text", x = 3500, y = -1.4, label = "Middle Item Fixated", colour = "mediumpurple4", size = 5) +
  annotate("text", x = 6000, y = -1.4, label = "Right Item Fixated", colour = "firebrick2", size = 5) +
  geom_point(data= data_new[time_step == first, list(time_step,value, variable)][3], colour = "navyblue", size = 3) +
  annotate("text", x = 6000, y = 1.1, label = "Right Item Chosen", size = 5,colour = "navyblue") + xlab("Time Step") + ylab("Relative Evidence Accumulated") +
  theme(legend.title=element_blank(), legend.position = "bottom", text = element_text(size = 20))
ggsave("c1_driftrates.pdf")



ggplot(data = data_new, aes(x = time_step, y = value, colour = variable, group = variable)) + geom_line() + ylim(c(-1.5,1.5)) +
  scale_colour_manual(breaks = unique(data_new[,variable]), values = c("red","green4","royalblue")) +
  geom_hline(yintercept = 1, colour = "black") + geom_vline(xintercept = 2000, linetype = "dashed") +
  geom_vline(xintercept = 5000, linetype = "dashed") + annotate("text", x = 700, y = 1.1, label = "decision threshold", colour = "black", size = 5) +
  annotate("text", x = 700, y = -1.4, label = "left item fixated", colour = "red", size = 5) +
  annotate("text", x = 3500, y = -1.4, label = "middle item fixated", colour = "green4", size = 5)  +
  annotate("text", x = 6000, y = -1.4, label = "right item fixated", colour = "royalblue", size = 5)  +
  geom_point(data= data_new[time_step == first, list(time_step,value, variable)][3], colour = "midnightblue", size = 3) +
  annotate("text", x = 6000, y = 1.1, label = "right item chosen", colour = "midnightblue", size = 5)  + xlab("time step") + ylab("relative evidence accumulated") +
  theme(legend.title=element_blank()) +theme(axis.title=element_text(size=14,face="bold")) + theme(legend.text=element_text(size=14))
ggsave("figure2_pres.png", width = 8, height =5.779468)


