setwd("C:/Anna/thesis/ADDM")
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
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.text.y = element_text(size = 12,face="bold"),
        legend.text=element_text(size=12),
        legend.title = element_blank()) + facet_grid(Transformation~Critical) +
  scale_colour_manual(values = c("darkorange2", "mediumpurple4"))+
labs(x = "Normalised value")
ggsave("explain.pdf")


rm(list = ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(gridExtra)
#setwd("C:/Anna/addm_writeup")
detect.better.than.second.best <- function(E1, E2, E3, threshold = 1) {
  e.sorted <- sort(c(E1, E2, E3), decreasing = TRUE)
  (e.sorted[1] - e.sorted[2]) > threshold
}
detect.better.than.average <- function(E1, E2, E3, threshold = 1) {
  e.sorted <- sort(c(E1, E2, E3), decreasing = TRUE)
  (e.sorted[1] - mean(e.sorted)) > threshold
}
detect.better.than.average.23 <- function(E1, E2, E3, threshold = 1) {
  e.sorted <- sort(c(E1, E2, E3), decreasing = TRUE)
  (e.sorted[1] - mean(e.sorted[2:3])) > threshold
}
E.grid <- data.table(expand.grid(E1 = 0:30/10, E2 = 0:30/10, E3 = 0:30/10))
E.grid[, `:=`(E12.diff, E1 - E2)]
E.grid[, `:=`(E13.diff, E1 - E3)]
E.grid[, `:=`(E23.diff, E2 - E3)]
E.grid[, `:=`(next.best.rule, mapply(detect.better.than.second.best, E1, E2, E3))]
E.grid[, `:=`(average.all.rule, mapply(detect.better.than.average, E1, E2, E3))]
E.grid[, `:=`(average.23.rule, mapply(detect.better.than.average.23, E1, E2, E3))]
E.grid.wide <- data.table(gather(E.grid, rule, finished,ends_with(".rule")))
E.grid.wide[, rule := ifelse(rule =="average.23.rule", "Average of 23",
                             ifelse(rule == "average.all.rule", "Average of all", "Next Best"))]
E.grid.wide[, `:=`(which.won, apply(cbind(E1, E2, E3), 1, which.max))]
E.grid.wide[, Which.won := ifelse(finished == TRUE, c("Alternative 1", "Alternative 2",
                                                      "Alternative 3")[which.won], "Not finished")]

E.grid.wide[rule == "Average of 23", rule := "Average of other two"]
E.grid.wide[rule == "Next Best", rule := "Next best"]

ggplot(E.grid.wide[rule != "Average of all",], aes (E12.diff, E13.diff, colour = Which.won)) + 
  theme_bw() +
  geom_point(size = 1) +
  facet_wrap(~rule) + theme(legend.position = "bottom",
                            strip.text.x = element_text(size = 18), 
                            text = element_text(size = 20),
                            strip.background = element_rect(fill=NA)) + 
  scale_colour_manual(values = c("sienna1", "firebrick", "dodgerblue2", "black")) + 
  labs(y=expression(E[1]-E[3]), x =expression(E[1]-E[2]), colour = "Which chosen")
ggsave("rulesfinished.png")


N <- 100000
sigma <- 1
X <- data.table(x=rnorm(N, sd=sigma), y=rnorm(N, sd=sigma), z=rnorm(N, sd=sigma))
X[,x.minus.y:=x-y]
X[,x.minus.z:=x-z]
arrowdata1 <- data.table(x = c(-2.5, -2.5), y = c(-2.5, 2.5), vx = c(2.5, 2.5), vy = c(2.5,-2.5))

p1 <- ggplot(X, aes(x.minus.y, x.minus.z))  + theme_bw() +
  xlim(c(-5, 5)) + ylim(c(-5, 5)) + geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) + 
  coord_fixed()+ geom_point(size = 1, colour = "mediumslateblue", alpha = 0.1)+
  geom_segment(data=arrowdata1, mapping=aes(x=x, y=y, xend=vx, yend=vy), col = "maroon4",arrow=arrow(), size = 1)+
  labs(y=expression(E[1]-E[3]), x =expression(E[1]-E[2])) + geom_density_2d(colour = "firebrick4")+
  theme(axis.title.x =  element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
  axis.title.y =  element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  #theme(plot.margin=unit(c(1,1,1,2),"cm"))

X[,x.minus.y.z.av:=x-(y+z)/2]
X[,z.minus.y.over.2:=(z-y)/2]
arrowdata2 <- data.table(x = c(-2.5, 0), y = c(0, 2.5), vx = c(2.5, 0), vy = c(0,-2.5))

p2 <- ggplot(X, aes(x.minus.y.z.av, z.minus.y.over.2))  + theme_bw() +
  xlim(c(-5, 5)) + ylim(c(-5, 5)) + geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) + 
  coord_fixed()+ geom_point(size = 1, colour = "mediumslateblue", alpha = 0.1)+
  geom_segment(data=arrowdata2, mapping=aes(x=x, y=y, xend=vx, yend=vy), col = "maroon4",arrow=arrow(), size = 1)+
  labs(y=expression(frac(E[2]-E[3],2)), x =expression(E[1]-frac(E[2]+E[3],2)))+ 
  geom_density_2d(colour = "firebrick4")

library(gridExtra)
margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))
grid.arrange(grobs = lapply(p1, "+", p2, margin))

# gA <- ggplotGrob(p1)
# gB <- ggplotGrob(p2)
# maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
# gA$widths[2:5] <- as.list(maxWidth)
# gB$widths[2:5] <- as.list(maxWidth)
# grid.arrange(gA, gB, ncol=2)

p3 <- grid.arrange(p1,p2, ncol = 2)
#ggsave("rotate.png")
########################################

##############

rm(list = ls())
make.delta.matrix <- function(ratings, d, theta) {
  # First first row is delta when attention is to Alternative 1, the second row for attention to Alternative 2, ...
  THETA <- 1-(1-diag(3))*(1-theta) 
  transform.matrix <- matrix(c(1,-.5,-.5,0,.5,-.5), byrow=TRUE, ncol=3) 
  delta.matrix <- t(d * transform.matrix %*% (THETA*ratings))
  colnames(delta.matrix) <- c("x", "y")
  delta.matrix
}


is.finished <- function(x,y) {
  if(x>1 | 3*y>(x+2) | 3*y< -(x+2)) {
    E <- c(x+y, 2*y, 0)
    return(which.max(E))
  } else
    return(0)
}

state.labels <- c("Not Finished", "Alternative 1", "Alternative 2", "Alternative 3")

make.xy.grid <- function(grid) {
  # NB Ordering of x and y important for xy.grid to always have y changing fast and x changing slow
  xy.grid <- CJ(x=seq(grid$x$lwr , grid$x$upr, grid$spacing), y=seq(grid$y$lwr , grid$y$upr, grid$spacing))
  xy.grid[,finished:=mapply(is.finished, x, y)]
  xy.grid[,finished:=factor(finished, levels=0:3, labels=state.labels)]
  xy.grid
}


make.bin.edges <- function(limit, spacing) {
  c(-Inf, seq(limit$lwr+spacing/2, limit$upr-spacing/2, spacing), Inf)
}

blur.from.xy <- function(x, y, sigma, x.bin.edges, y.bin.edges) {
  # Makes a matrix for each point in the xy.grid 
  p.x <- diff(pnorm(x.bin.edges, x, sd= sigma*sqrt(3/2))) # sqrt(3/2) and sqrt(1/2) from covariance matrix
  p.y <- diff(pnorm(y.bin.edges, y, sd= sigma*sqrt(1/2)))
  outer(p.y ,p.x)
}


make.blur.matrix <- function(grid, delta, sigma) {
   x.bin.edges <- make.bin.edges(grid$x, grid$spacing)
   y.bin.edges <- make.bin.edges(grid$y, grid$spacing)
  xy.grid <- make.xy.grid(grid)
  mapply(blur.from.xy, xy.grid$x+delta["x"], xy.grid$y+delta["y"],
         MoreArgs=list(sigma, x.bin.edges, y.bin.edges))
}

renormalize <- function(xy.grid, not.finished.prob) {
  xy.grid[finished!="Not Finished", prob:=0]
  #xy.grid[,prob:=ifelse(not.finished.prob != 0, prob/not.finished.prob,0), by = 1:nrow(xy.grid)]
  #xy.grid[,prob:=prob/not.finished.prob]
  xy.grid
}

exit.probabilities <- function(xy.grid) {
  exit.probs <- xy.grid[,.(prob=sum(prob)),by=.(finished)][order(finished),]$prob
  names(exit.probs) <- levels(xy.grid$finished)
  exit.probs
}

############
grid <- list(spacing=0.1, x=list(lwr=-2.5, upr=1.5), y=list(lwr=-1.5, upr=1.5))

xy.grid <- make.xy.grid(grid)
xy.grid[,prob:=0]
xy.grid[x==0 & y==0, prob:=1]

ratings <- c(0.5,0.5,0.5)
d <- 1
theta <- 2/3
(  delta <- make.delta.matrix(ratings, d, theta)  )
sigma <-  0.3
#sigma <- sigma*d

#sigma <- sigma
#sigma <- sigma*soi

blur.matrices <- list(
  attend.1=make.blur.matrix(grid, delta=delta[1,], sigma),
  attend.2=make.blur.matrix(grid, delta=delta[2,], sigma),
  attend.3=make.blur.matrix(grid, delta=delta[3,], sigma)
)


##############################
# library(reshape2)
# melted_cormat <- melt(blur.matrices[[1]])
# head(melted_cormat)
# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()+ scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")

##############################

plot.xy.grid <- function(xy.grid, grid) {
  threshold.triangle <- data.frame(x=c(-2,1,1), y=c(0,1,-1)) # equivalent to hard coding of threshold 1
  x.bin.edges <- make.bin.edges(grid$x, grid$spacing)
  y.bin.edges <- make.bin.edges(grid$y, grid$spacing)
  
  # ggplot(xy.grid, aes(x, y, fill=prob)) + 
  #   geom_tile(width=0.9*grid$spacing, height=0.9*grid$spacing) + 
  #   coord_equal() + 
  #   labs(x=expression(x==E^1-frac(E^2+E^3,2)), y=expression(y==frac(E^3-E^2,2)), fill="Probability") + 
  #   geom_polygon(data=threshold.triangle, aes(x,y), fill="yellow", alpha=0.3, col=NA) +
  #   scale_x_continuous(minor_breaks=x.bin.edges) + scale_y_continuous(minor_breaks=y.bin.edges)# + 
  #theme(panel.grid.minor=element_line(colour="green", size=0.1))
  ggplot(xy.grid, aes(x, y)) + geom_tile(aes( fill=finished),alpha = 0.5) + geom_point(aes(col = prob)) +
    scale_fill_manual(values = c("rosybrown", "sienna1","firebrick","dodgerblue2"))+ 
    labs(x=expression(x==E^1-frac(E^2+E^3,2)), y=expression(y==frac(E^3-E^2,2)), col="Probability")+scale_color_gradient(low="navyblue", high="yellow") + guides(fill = guide_legend("States")) + theme_bw()
}


#attention <- c(1,1,1,1,1,1,1,1)
#attention <- rep(3, 8)# Which alternative is attended in each frame
attention <- c(1,1,2,2,2,3,3)
grid <- list(spacing=0.1, x=list(lwr=-2.5, upr=1.5), y=list(lwr=-1.5, upr=1.5))

xy.grid <- make.xy.grid(grid)
xy.grid[,prob:=0]
xy.grid[x==0 & y==0, prob:=1]

xy.grid[,f:=0]
xy.grid.animation <- copy(xy.grid)
exit.probs <- data.table(f=0, state=state.labels, prob=c(1,0,0,0))
data <- list()
data[[1]] <- xy.grid.animation
for(i in 1:length(attention)) {
  xy.grid[, prob:=blur.matrices[[attention[i]]] %*% prob]
  xy.grid[,f:=i]
  xy.grid.animation <- rbind(xy.grid.animation, xy.grid)
  ep <- exit.probabilities(xy.grid)
  exit.probs <- rbind(exit.probs, data.table(f=i, state=names(ep), prob=ep))
  xy.grid <- renormalize(xy.grid, ep["Not Finished"])
  data[[i+1]] <- xy.grid.animation[f==i,]
  #plots[[i]] <- 	plot.xy.grid(xy.grid.animation[f==i,], grid)
  print(i)
}
#xy.grid.animation[,f:=as.factor(f)]

to.plot <- data.table(do.call(rbind.data.frame, data))

probcurves <- to.plot[,list(prob = sum(prob)),.(finished, f)]

p2 <- ggplot(probcurves, aes(f,prob, group = finished, col = finished)) + theme_bw()+
  geom_point() +
  geom_line(size = 1)+
  scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2")) +
  labs(colour = "Finished", y = "Probability", x = "Time step") + 
  theme(text = element_text(size = 20))+ theme(aspect.ratio = 0.62)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

# p1 <- ggplot(to.plot, aes(x, y)) + geom_tile(aes( fill=prob),alpha = 0.5) + 
#   geom_point(aes(col = finished), alpha = 0.2) + theme_bw() + 
#   facet_wrap(~ f, ncol = 4) +
#   scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2"))+ 
#   scale_fill_gradient(high = "darkorange1", low = "midnightblue")+ 
#   labs(fill = "Probability", colour = "Finished",
#        y=expression(frac(E[2]-E[3],2)), x =expression(E[1]-frac(E[2]+E[3],2)))+
#   theme(text = element_text(size = 25))

to.plot[, Time.step := paste("Time step", f)]

p1 <- ggplot(to.plot, aes(x, y)) + geom_tile(aes( fill=prob),alpha = 0.5) + 
  geom_point(aes(col = finished), alpha = 0.3) + theme_bw() + 
  facet_wrap(~ Time.step, ncol = 4) +
  scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2"))+ 
  #scale_fill_gradient(high = "yellow", low = "navyblue")+ 
  scale_fill_gradientn(values=c(1, 0.75, 0.5, 0.25, 0.1, 0.05, 0.01, 0.008, 0.006, 0.004, 0.002, 0.001, 0.0005,0),
                       colours=c(rev(c("navyblue", "darkblue","blue4", "blue3","royalblue", "royalblue1", "royalblue2",
                                 "cornflowerblue","dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4")), "orange"))+
                      
  labs(fill = "Probability", colour = "Finished", y= "", x = "")+
  theme(text = element_text(size = 20),
        strip.background = element_rect(fill=NA))+ guides(colour=FALSE) + theme(aspect.ratio = 0.9)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))


#grid.arrange(grobs = lapply(pl, "+", margin))

p3 <- grid.arrange(p1,p2, ncol = 1)


########################
rm(list=ls())
library(data.table)
load("NM_results.RData")
grid_results_NM_second <- data.table(rbind(
  t(sapply(1:50, function (x) unlist(pars_divided_sec[[x]])[c(1,2,3,4)])),
  t(sapply(1:50, function (x) unlist(pars_range_sec[[x]])[c(1,2,3,4)])),
  t(sapply(1:50, function (x) unlist(pars_rank_sec[[x]])[c(1,2,3,4)])),
  t(sapply(1:50, function (x) unlist(pars_max_sec[[x]])[c(1,2,3,4)]))
))
grid_results_NM_second[, Type := rep(c("Objective_divided", "Range", "Rank", "Max"), each = 50)]
grid_results_NM_second[, Subjectno := rep(seq(1,50),4)]

setnames(grid_results_NM_second, old = c('par1','par2','par3','value'), new = c('theta_NM_sec','sigma_NM_sec','soi_NM_sec','loglik_NM_sec'))


table(grid_results_NM_second[, Type[which.max(loglik_NM_sec)] , by = Subjectno][,V1])

library(xtable)
setnames(grid_results_NM_second, colnames(grid_results_NM_second),
         c("theta", "sigma", "soi", "loglik", "Type", "Subjectno"))
grid_results_NM_second[Type == "Objective_divided", Type := "Global Max" ]
grid_results_NM_second[Type == "Max", Type := "Local Max" ]
bestfit <- grid_results_NM_second[, Type[which.max(loglik)] , by = Subjectno]
grid_results_NM_second<- melt(grid_results_NM_second, id.vars = c("Subjectno", "Type"), measure.vars = c("theta", "sigma", "soi"))
grid_results_NM_second <- dcast(grid_results_NM_second, Subjectno ~ Type + variable)
grid_results_NM_second <- merge(bestfit, grid_results_NM_second, by = "Subjectno",all.x = T)


xtable(grid_results_NM_second[, -c("Subjectno")],type = "latex")



#sim_res <- data.table(dcast(grid_results_NM_second, Subjectno ~ Type, value.var = "loglik_NM_sec"))
# 
# ggpairs(grid_results_NM_second,
#         title="Parameter distribution, simulations approach",
#         columns = 1:3,
#         mapping=ggplot2::aes(colour = Type))
round(grid_results_NM_second[ , lapply(.SD, median), .SDcols = colnames(grid_results_NM_second)[3:14]],2)

load("Results_finalfinal.RData")
setnames(results, c("V1", "V2", "V3"), c("theta_NM", "sigma_NM", "soi_NM"))
table(results[, Type[which.max(V4)] , by = Subjno][,V1])

bestfit <- results[, Type[which.max(V4)] , by = Subjno]
#tri_res <- data.table(dcast(results, Subjno ~ Type, value.var = "loglikelihood"))
results<- melt(results, id.vars = c("Subjno", "Type"), measure.vars = c("theta_NM", "sigma_NM", "soi_NM"))
results[, Type:= factor(Type, levels = c("Objective_divided", "Max", "Range", "Rank"))]

results <- dcast(results, Subjno ~ Type + variable)
results <- merge(bestfit, results, by = "Subjno",all.x = T)

results[V1 == "Objective_divided", V1 := "Global Max" ]
results[V1 == "Max", V1 := "Local Max" ]

print(xtable(results[, -c("Subjno")],
             type = "latex",include.rownames=FALSE))

round(results[ , lapply(.SD, median), .SDcols = colnames(results)[3:14]],2)
# library("GGally")
# ggpairs(results[,c("theta_NM", "sigma_NM", "soi_NM")])
# ggpairs(results,
#         title="Parameter distribution, triangle approach",
#         columns = 7:9,
#         mapping=ggplot2::aes(colour = Type))
# 
# table(results[, Type[which.max(V4)] , by = Subjno][,V1])
###################################################################################################
#EXPERIMENT 2 QUALITATIVE COMPARISON
##########################################################
rm(list = ls())
library(Rmisc)
library(DescTools)
library(ggplot2)
library(gridExtra)
library(data.table)
library(lattice)
library(grid)
source("simulations.R")
setwd("C:/Anna/thesis/ADDM")
load("merged_and_cleaned.RData")




Subj <- function(obj) {
  range <- sapply(obj, function(x) (x-min(obj))/(max(obj)-min(obj)))
  max <- sapply(obj, function(x) x/max(obj))
  rank <- sapply(obj, function(x) ifelse(x == max(obj),1,ifelse(x == min(obj),0,0.5)))
  obj <- sapply(obj, function(x) x/7)
  cbind(obj,range,max,rank)
}


complete_fixations[, Best.rating := c(Item1_rating, Item2_rating, Item3_rating)[best], by = 1:nrow(complete_fixations)]
complete_fixations[, Middle.rating := c(Item1_rating, Item2_rating, Item3_rating)[middle], by = 1:nrow(complete_fixations)]
complete_fixations[, Worst.rating := c(Item1_rating, Item2_rating, Item3_rating)[worst], by = 1:nrow(complete_fixations)]
complete_fixations[, valueset := paste(c(Worst.rating, Middle.rating, Best.rating), collapse = ""), by = 1:nrow(complete_fixations)]
complete_fixations[, which.chosen := Which.chosen]


get.simulations <- function(item1, item2, item3, speed.of.int,noise, no.sim, which) {
# item1 <- c(1,2)
# item2 <- c(2,4)
# item3 <- c(3,6)
# noise <- 0.25
# speed.of.int <- 0.15
# which <- 1
# no.sim <- 10000

values <- data.table(rbind(t(Subj(c(item1[1],item2[1],item3[1]))),
                          t(Subj(c(item1[2],item2[2],item3[2])))))

objdiv <- c(as.numeric(round(values[1,],2)),as.numeric(round(values[5,],2)))
range <-  c(as.numeric(round(values[2,],2)),as.numeric(round(values[6,],2)))
max <-  c(as.numeric(round(values[3,],2)),as.numeric(round(values[7,],2)))
rank <-  c(as.numeric(round(values[4,],2)),as.numeric(round(values[8,],2)))

all <- list(objdiv, range, max, rank)


sims <- data.table(rbind(get.sim(item1_r = all[[which]][1], item2_r =all[[which]][2], item3_r = all[[which]][3], noise = noise, speed.of.int = speed.of.int, no.sim = no.sim),
             get.sim(item1_r = all[[which]][4], item2_r =all[[which]][5], item3_r = all[[which]][6], noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)))
sims[, valueset := rep(c(paste(c(item1[1], item2[1], item3[1]), collapse = ""), paste(c(item1[2], item2[2], item3[2]), collapse = "")),each =  no.sim)]
sims[, Which.chosen := c("Worst", "Middle", "Best")[Chosen]]
counts <- sims[,.N,.(valueset, Which.chosen)]
counts <- merge(data.table(expand.grid(valueset = unique(counts$valueset), Which.chosen = c("Best", "Worst", "Middle"))),
                counts, by = c("valueset", "Which.chosen"), all.x = T)
counts[is.na(N), N:=0]
counts <- counts[order(valueset, Which.chosen)]

counts <- cbind(counts, rbind(MultinomCI(counts[1:3,N], conf.level=0.95, method="sisonglaz"),
                              MultinomCI(counts[4:6,N], conf.level=0.95, method="sisonglaz")))
counts[, Which.chosen := factor(Which.chosen, levels = c("Worst", "Middle", "Best"))]
return(counts)
# plot <- ggplot(counts, aes(x= Which.chosen, y = est, fill=factor(valueset))) + 
#   geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12))
# return(plot)
}
load("exp2_choiceRT.RData")

 all_data[, valueset2 := paste(sort((c(item1, item2, item3)-10)/10),  collapse = ""), by = 1:nrow(all_data)]
 all_data[, Item1_rating := (item1-10)/10, by = 1:nrow(all_data)]
 all_data[, Item2_rating := (item2-10)/10, by = 1:nrow(all_data)]
 all_data[, Item3_rating := (item3-10)/10, by = 1:nrow(all_data)]

#get empirical choice predictions
#item values are defined by experiment 1
 setnames(complete_fixations, "valueset", "valueset2")
get.empirical <- function(data) {
  values <- unique(all_data$valueset2)
  data <- data[valueset2 %in% values, ]
  counts <- data[,.N, .(valueset2, which.chosen)]
  counts <- merge(data.table(expand.grid(valueset2 = unique(counts$valueset2), which.chosen = c("best", "worst", "middle"))),
                  counts, by = c("valueset2", "which.chosen"), all.x = T)
  counts[is.na(N), N:=0]
  counts <- counts[order(valueset2, which.chosen)]
  
  counts <- cbind(counts, rbind(MultinomCI(counts[1:3,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[4:6,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[7:9,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[10:12,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[13:15,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[16:18,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[19:21,N], conf.level=0.95, method="sisonglaz")))
  return(counts)
}

empirical <- get.empirical(all_data)

get.simulations(c(1,4), c(2,5), c(4,7), 0.1, 0.1, 1000, 1)

#find best fitting parameter set for each value transformation rule
#based on the empirical fit to data from experiment 2
#say there's a 5x5x5 parameter grid
#pick best fitting for each rule and then use that for the qualitative comparison
# param.grid <- data.table(expand.grid(soi = c(0.15, 0.25, 0.35, 0.45, 0.55),
#                                      sigma = c(0.15, 0.25, 0.35, 0.45, 0.55),
#                                      valtr = c(1,2,3,4)))

#function that calculates loglik for one value transformation rule and parameter pair
setnames(empirical, "est", "est2")
get.sse <- function(par, valtr, data) {
 sigma <- par[1]
  soi <- par[2]
# valtr <- 1
# data <- all_data
  
 simulations <-  rbind(get.simulations(c(1,4), c(2,5), c(4,7), noise = sigma, speed.of.int = soi, 10000, valtr),
        get.simulations(c(1,2), c(2,4), c(3,6), noise = sigma, speed.of.int = soi, 10000, valtr),
        get.simulations(c(1,1), c(3,6), c(7,7), noise = sigma, speed.of.int = soi, 10000, valtr),
        get.simulations(c(1,5), c(6,6), c(7,7), noise = sigma, speed.of.int = soi, 10000, valtr))

 simulations[, Which.chosen := tolower(Which.chosen)]

 overall <- merge(simulations[, c("valueset", "Which.chosen", "est")],
                  empirical[, c("valueset2", "which.chosen", "est2")],
                  by.x = c("valueset", "Which.chosen"),
                  by.y = c("valueset2", "which.chosen"),
                  all.x = T)
 return(overall[, sum((est-est2)^2)])
}




# res1 <- optim(par = c(0.1, 0.1), fn = get.sse, data = all_data, valtr =1, control = list(trace = 6, maxit = 150))
# res1 <- optim(par = res1$par, fn = get.sse, data = all_data, valtr =1, control = list(trace = 6, maxit = 150))
# 
# 
# res2 <- optim(par = c(0.1, 0.1), fn = get.sse, data = all_data, valtr =2, control = list(trace = 6, maxit = 150))
# res2 <- optim(par = res2$par, fn = get.sse, data = all_data, valtr =2, control = list(trace = 6, maxit = 150))
# 
# 
# res3 <- optim(par = c(0.1, 0.1), fn = get.sse, data = all_data, valtr =3, control = list(trace = 6, maxit = 150))
# res3 <- optim(par = res3$par, fn = get.sse, data = all_data, valtr =3, control = list(trace = 6, maxit = 150))
# 
# 
# res4 <- optim(par = c(0.1, 0.1), fn = get.sse, data = all_data, valtr =4, control = list(trace = 6, maxit = 150))
# res4 <- optim(par = res4$par, fn = get.sse, data = all_data, valtr =4, control = list(trace = 6, maxit = 150))

#res1 0.11902563 0.08594647
#res2 0.14728225 0.05644857
#res3 0.12789600 0.07237432
#res4 0.15398266 0.06149204


#param.grid[, loglik := get.loglik(valtr = valtr, soi = soi, sigma = sigma), by = 1:nrow(param.grid)]
#best <- param.grid[, .SD[which.min(loglik)], .(valtr)]
#######################MAXIMUM LIKELIHOOD FOR EACH VALUE TRANSFORMATION RULE#####################

# all_data[, valueset2 := paste(sort((c(item1, item2, item3)-10)/10),  collapse = ""), by = 1:nrow(all_data)]
# all_data[, Item1_rating := (item1-10)/10, by = 1:nrow(all_data)]
# all_data[, Item2_rating := (item2-10)/10, by = 1:nrow(all_data)]
# all_data[, Item3_rating := (item3-10)/10, by = 1:nrow(all_data)]
# all_data <- cbind(all_data, t(apply(all_data[,c('Item1_rating','Item2_rating','Item3_rating'), with = FALSE],1,Subj)))
# setnames(all_data, old = c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12'), new =  c('Item1_obj', 'Item2_obj', 'Item3_obj', 'Item1_range','Item2_range', 'Item3_range', 'Item1_max', 'Item2_max', 'Item3_max','Item1_rank', 'Item2_rank', 'Item3_rank'))
# ##
# #create a function that calculates the probability
# 
# 
# 
# calcprob <- function(valueset, noise, soi, nosim) {
#   
# boo <- get.sim(, noise, soi, nosim)
# probs <- table(boo$Chosen)/nosim
# return(probs)
# #  ifelse(chosen %in% names(probs), as.numeric(probs[which(names(probs) == chosen,0)]),0)
# }
# 
# 
# 
# 
# sum(log(mapply(calcprob, item1 = as.vector(all_data$Item1_obj), item2 = as.vector(all_data$Item2_obj),
#                item3 = as.vector(all_data$Item3_obj), chosen = as.vector(all_data$Chosenitem),nosim = 100,
#                MoreArgs = list(noise = 0.1, soi = 0.1))))
# 
# log.likelihood <- function(p, type, no.of.sim) {
#   sigma <- p[1]
#   soi <- p[2]
#   if (type == "Objective") {
#     sum(log(mapply(calcprob, item1 = as.vector(all_data$Item1_obj), item2 = as.vector(all_data$Item2_obj),
#                    item3 = as.vector(all_data$Item3_obj), chosen = as.vector(all_data$Chosenitem),nosim = no.of.sim,
#                    MoreArgs = list(noise = sigma, soi = soi))))
#         } else if  (type == "Max") {
#         sum(log(mapply(calcprob, item1 = as.vector(all_data$Item1_max), item2 = as.vector(all_data$Item2_max),
#                        item3 = as.vector(all_data$Item3_max), chosen = as.vector(all_data$Chosenitem),nosim = no.of.sim,
#                        MoreArgs = list(noise = sigma, soi = soi))))  
#         } else if  (type == "Range") {
#         sum(log(mapply(calcprob, item1 = as.vector(all_data$Item1_range), item2 = as.vector(all_data$Item2_range),
#                          item3 = as.vector(all_data$Item3_range), chosen = as.vector(all_data$Chosenitem),nosim = no.of.sim,
#                          MoreArgs = list(noise = sigma, soi = soi))))  
#         } else if  (type == "Rank") {
#         sum(log(mapply(calcprob, item1 = as.vector(all_data$Item1_rank), item2 = as.vector(all_data$Item2_rank),
#                              item3 = as.vector(all_data$Item3_rank), chosen = as.vector(all_data$Chosenitem),nosim = no.of.sim,
#                              MoreArgs = list(noise = sigma, soi = soi))))  
#                 } 
# }

########################################################################
#for each choice set man
setwd("C:/Anna/thesis/ADDM")
load("merged_and_cleaned.RData")
# complete_fixations[, Best.rating := c(Item1_rating, Item2_rating, Item3_rating)[best], by = 1:nrow(complete_fixations)]
# complete_fixations[, Middle.rating := c(Item1_rating, Item2_rating, Item3_rating)[middle], by = 1:nrow(complete_fixations)]
# complete_fixations[, Worst.rating := c(Item1_rating, Item2_rating, Item3_rating)[worst], by = 1:nrow(complete_fixations)]
# complete_fixations[, valueset := paste(c(Worst.rating, Middle.rating, Best.rating), collapse = ""), by = 1:nrow(complete_fixations)]
# complete_fixations[, which.chosen := Which.chosen]


complete_fixations[, Best.rating := c(Item1_rating, Item2_rating, Item3_rating)[best], by = 1:nrow(complete_fixations)]
complete_fixations[, Middle.rating := c(Item1_rating, Item2_rating, Item3_rating)[middle], by = 1:nrow(complete_fixations)]
complete_fixations[, Worst.rating := c(Item1_rating, Item2_rating, Item3_rating)[worst], by = 1:nrow(complete_fixations)]
complete_fixations[, valueset := paste(c(Worst.rating, Middle.rating, Best.rating), collapse = ""), by = 1:nrow(complete_fixations)]
complete_fixations[, which.chosen := Which.chosen]
setnames(complete_fixations, "valueset", "valueset2")

library(Hmisc)

make.plot <- function(item1, item2, item3) {
  # item1 <- c(1,5)
  # item2 <- c(6,6)
  # item3 <- c(7,7)
  plot1 <- get.simulations(item1, item2, item3, speed.of.int = 0.09, noise = 0.12, 100000, 1)
  plot2 <- get.simulations(item1, item2, item3, speed.of.int = 0.06, noise = 0.15, 100000, 2)
  plot3 <- get.simulations(item1, item2, item3, speed.of.int = 0.07, noise = 0.13, 100000, 3)
  plot4 <- get.simulations(item1, item2, item3, speed.of.int = 0.06, noise = 0.15, 100000, 4)
  values <- data.table(rbind(t(Subj(c(item1[1],item2[1],item3[1]))),
                             t(Subj(c(item1[2],item2[2],item3[2])))))
  
  objdiv <- c(as.numeric(round(values[1,],2)),as.numeric(round(values[5,],2)))
  range <-  c(as.numeric(round(values[2,],2)),as.numeric(round(values[6,],2)))
  max <-  c(as.numeric(round(values[3,],2)),as.numeric(round(values[7,],2)))
  rank <-  c(as.numeric(round(values[4,],2)),as.numeric(round(values[8,],2)))
  
  
  p1 <- ggplot(plot1, aes(x= Which.chosen, y = est, fill=factor(valueset))) + theme_bw()+
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1)) + theme(legend.text=element_text(size=12)) +
    #  geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") + 
    #labs(title=paste("Global Max,\n",objdiv[1], objdiv[2], objdiv[3], "\n",objdiv[4], objdiv[5], objdiv[6]),fill="") +
    labs(title=paste("\nGlobal Max"),fill="") +
    theme(plot.title = element_text(size=13,hjust=0.5)) + theme(legend.position="bottom") + ylab(c(" ")) + 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("") +
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))+ theme(legend.text=element_text(size=19))
  
  p4 <- ggplot(plot2, aes(x= Which.chosen, y = est, fill=factor(valueset))) + theme_bw()+
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) + 
    #   geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") + 
    #labs(title=paste("Range,\n",range[1], range[2], range[3], "\n",range[4], range[5], range[6]),fill="")+
    labs(title=paste("\nRange"),fill="") +
    theme(plot.title = element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))
  
  p2 <- ggplot(plot3, aes(x= Which.chosen, y = est, fill=factor(valueset))) + theme_bw()+
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) +
    #   geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") +
    #labs(title=paste("Local Max,\n",max[1], max[2], max[3], "\n",max[4], max[5], max[6]),fill="")+
    labs(title=paste("\nLocal Max"),fill="") +
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))
  
  
  p3 <- ggplot(plot4, aes(x= Which.chosen, y = est, fill=factor(valueset))) + theme_bw()+
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) +
    #  geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") +
    #labs(title=paste("Rank,\n",rank[1], rank[2], rank[3], "\n",rank[4], rank[5], rank[6]),fill="")+
    labs(title=paste("\nRank"),fill="") +
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))
  
  
  
 

  plot5 <- get.empirical(complete_fixations)
  plot5 <- plot5[valueset2 == paste(c(item1[1], item2[1], item3[1]), collapse = "")| 
         valueset2 == paste(c(item1[2], item2[2], item3[2]), collapse = ""),]
  plot5[, Which.chosen := capitalize(which.chosen)]
  plot5[, Which.chosen := factor(Which.chosen, levels = c("Worst", "Middle", "Best"))]
  plot6 <- get.empirical(all_data)
  plot6 <- plot6[valueset2 == paste(c(item1[1], item2[1], item3[1]), collapse = "")| 
                   valueset2 == paste(c(item1[2], item2[2], item3[2]), collapse = ""),]
  plot6[, Which.chosen := capitalize(which.chosen)]
  plot6[, Which.chosen := factor(Which.chosen, levels = c("Worst", "Middle", "Best"))]
  
  p5 <- ggplot(plot5, aes(x = Which.chosen,y =est, fill=as.factor(valueset2)))+ theme_bw()+
    geom_bar(stat="identity", position = "dodge") + geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = position_dodge(width = 0.9), width=0.2) +
    ylim(c(0,1)) + labs(title = paste("Data\nExperiment 1"), fill = "") + theme(legend.position="bottom")  + theme(legend.text=element_text(size=12)) +
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))
  
  p6 <- ggplot(plot6, aes(x = Which.chosen,y =est, fill=as.factor(valueset2)))+ theme_bw()+
    geom_bar(stat="identity", position = "dodge") + geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = position_dodge(width = 0.9), width=0.2) +
    ylim(c(0,1)) + labs(title = paste("Data\nExperiment 2"), fill = "") + theme(legend.position="bottom")  + theme(legend.text=element_text(size=12)) +
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=18))
  
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(p1)
  
  
  predictions <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                          p2 + theme(legend.position="none"),
                                          p5 + theme(legend.position="none"),
                                          p3 + theme(legend.position="none"),
                                          p4 + theme(legend.position="none"),
                                          p6 + theme(legend.position="none"),
                                          # top = textGrob("Predictions", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
                                          left = textGrob("Choice Proportion", rot = 90, vjust = 1, gp = gpar(fontsize = 18)),
                                          # 
                                          nrow=2),mylegend, heights=c(10, 1))
  
}


addconst <- make.plot(c(1,4), c(2,5), c(4,7))
ggsave("addconst.pdf")
multiply <- make.plot(c(1,2), c(2,4), c(3,6))
ggsave("multiply.pdf")
closedistsec <- make.plot(c(1,1), c(3,6), c(7,7))
ggsave("closedistsec.pdf")
closedistthird <- make.plot(c(1,5), c(6,6), c(7,7))
ggsave("closedistthird.pdf")







