setwd("C:/Anna/addm_writeup")
library(data.table)
library(ggplot2)

set.seed(10)

n <- 1000
x <- cumsum(sample(c(-0.1, 0.1), n, TRUE))

data <- data.table("Time_step" = c(seq(1:1000)), "Relative_Evidence" = x) 

ggplot(data = data[Time_step <= 721], aes(x = Time_step, y = Relative_Evidence)) + 
  theme_bw()+
  geom_line() + 
  geom_hline(yintercept = 2.5, colour ="mediumpurple4") + 
  geom_hline(yintercept = -2.5, colour ="darkorange2") + xlim(c(0,850)) + ylim(c(-3,3)) +
  annotate("text", x = 200, y = 2.7,size = 5, label = "Decision threshold for choosing option A", colour = "mediumpurple4")+
  annotate("text", x = 200, y = -2.7,size = 5, label = "Decision threshold for choosing option B", colour = "darkorange2") +
  geom_vline(xintercept = 721, linetype = "dashed") + 
  geom_point(data= data[Time_step == 721,], colour = "navyblue", size = 3) +
  xlab("Time step") + ylab("Relative evidence accumulated") +
  annotate("text", x = 800, y = 2.3, label = "Option A chosen", colour = "navyblue", size = 5) +
  theme(text = element_text(size = 20))
ggsave("c1_randomwalk.pdf")


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


ggplot(data = data_new, aes(x = time_step, y = value, colour = variable, group = variable)) +
  theme_bw() +
  geom_line(size = 1.5) + ylim(c(-1.5,1.5)) +
  scale_colour_manual(breaks = unique(data_new[,variable]), values = c("darkorange2","mediumpurple4","firebrick2")) +
  geom_hline(yintercept = 1, colour = "black") + geom_vline(xintercept = 2000, linetype = "dashed") +
  geom_vline(xintercept = 5000, linetype = "dashed") + annotate("text", x = 700, y = 1.1, label = "Decision threshold", colour = "black", size = 5) +
  annotate("text", x = 700, y = -1.4, label = "Left item fixated", colour = "darkorange2", size = 5) +
  annotate("text", x = 3500, y = -1.4, label = "Middle item fixated", colour = "mediumpurple4", size = 5) +
  annotate("text", x = 6000, y = -1.4, label = "Right item fixated", colour = "firebrick2", size = 5) +
  geom_point(data= data_new[time_step == first, list(time_step,value, variable)][3], colour = "navyblue", size = 3) +
  annotate("text", x = 6000, y = 1.1, label = "Right item chosen", size = 5,colour = "navyblue") + xlab("Time step") + ylab("Relative evidence accumulated") +
  theme(legend.title=element_blank(), legend.position = "bottom", text = element_text(size = 20))
ggsave("c1_driftrates.pdf")



Subj <- function(obj) {
  range <- sapply(obj, function(x) (x-min(obj))/(max(obj)-min(obj)))
  max <- sapply(obj, function(x) x/max(obj))
  rank <- sapply(obj, function(x) ifelse(x == max(obj),1,ifelse(x == min(obj),0,0.5)))
  # range_globmax <- sapply(obj, function(x) (x-min(obj))/(7-min(obj)))
  obj <- sapply(obj, function(x) x/7)
  cbind(obj,range,max,rank)
}

dataset <- data.table(rbind(t(Subj(c(1,2,4))),
                            t(Subj(c(4,5,7))),
                            t(Subj(c(1,2,3))),
                            t(Subj(c(2,4,6))),
                            t(Subj(c(1,3,7))),
                            t(Subj(c(1,6,7))),
                            t(Subj(c(1,6,7))),
                            t(Subj(c(5,6,7)))))

dataset[, valueset := as.character(rep(c(124,457,123,246,137,167,167,567), each = 4))]
setnames(dataset, old = c("V1", "V2", "V3"), new = c("item1", "item2", "item3"))
dataset[, Transformation := rep(c("Global Max", "Range", "Local Max", "Rank"),8)]
dataset[, Transformation := factor(Transformation, levels = c("Range", "Rank", "Local Max", "Global Max"))]
dataset[, Critical := rep(c("Adding a constant\n Original: 124, New: 457",
                            "Multiplying by a constant\n Original: 123, New: 246", 
                            "Distant vs close second\n Original: 137, New: 167", 
                            "Distant vs close third\n Original: 167, New: 567"),each = 8)]
dataset[, Critical := factor(Critical, levels =c("Adding a constant\n Original: 124, New: 457",
                                                 "Multiplying by a constant\n Original: 123, New: 246", 
                                                 "Distant vs close second\n Original: 137, New: 167", 
                                                 "Distant vs close third\n Original: 167, New: 567"))]
dataset[, Type := rep(rep(c("Original", "New"),each = 4),4)]
dataset[, Type:= factor(Type, levels = c("Original", "New"))]
dataset <- melt(dataset, measure.vars = c("item1", "item2", "item3"),
                variable.name = "item", value.name = "value")
dataset[,phonyy := rep(c(1.05,1.05,1.05,1.05,1,1,1,1), 12)]


ggplot(dataset,aes(x = value, y = phonyy, group =Type, colour = Type)) + geom_point(size = 2) + theme_bw() +
  ylim(c(0.99,1.06))  + theme(legend.position="bottom") +
  scale_x_continuous(breaks=c(0,0.5,1)) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.text.y = element_text(size = 12,face="bold"),
        legend.text=element_text(size=12),
        legend.title = element_blank()) + facet_grid(Transformation~Critical) +
  scale_colour_manual(values = c("darkorange2", "mediumpurple4"))
ggsave("explain.pdf")








