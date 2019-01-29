rm(list = ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(gridExtra)
setwd("C:/Anna/addm_writeup")
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



ggplot(E.grid.wide[rule != "Average of all",], aes (E12.diff, E13.diff, colour = Which.won)) + geom_point(size = 1) +
         facet_wrap(~rule) + theme(legend.position = "bottom",
                              strip.text.x = element_text(size = 18), text = element_text(size = 20)) + 
  scale_colour_manual(values = c("sienna1", "firebrick", "dodgerblue2", "black")) + 
  labs(y=expression(E[1]-E[3]), x =expression(E[1]-E[2]), colour = "Which won")
ggsave("rulesfinished.pdf")


#demonstrate rotation
N <- 1e4
sigma <- 1
X <- data.table(x=rnorm(N, sd=sigma), y=rnorm(N, sd=sigma), z=rnorm(N, sd=sigma))
X[,x.minus.y:=x-y]
X[,x.minus.z:=x-z]
arrowdata1 <- data.table(x = c(-2.5, -2.5), y = c(-2.5, 2.5), vx = c(2.5, 2.5), vy = c(2.5,-2.5))

p1 <- ggplot(X, aes(x.minus.y, x.minus.z))  +
  xlim(c(-5, 5)) + ylim(c(-5, 5)) + geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) + 
  coord_fixed()+ geom_point(size = 1, colour = "grey40")+
  geom_segment(data=arrowdata1, mapping=aes(x=x, y=y, xend=vx, yend=vy), col = "red4",arrow=arrow(), size = 1)+
  labs(y=expression(E[1]-E[3]), x =expression(E[1]-E[2]))
  
X[,x.minus.y.z.av:=x-(y+z)/2]
X[,z.minus.y.over.2:=(z-y)/2]
arrowdata2 <- data.table(x = c(-2.5, 0), y = c(0, 2.5), vx = c(2.5, 0), vy = c(0,-2.5))

p2 <- ggplot(X, aes(x.minus.y.z.av, z.minus.y.over.2))  +
  xlim(c(-5, 5)) + ylim(c(-5, 5)) + geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) + 
  coord_fixed()+ geom_point(size = 1, colour = "grey40")+
  geom_segment(data=arrowdata2, mapping=aes(x=x, y=y, xend=vx, yend=vy), col = "red4",arrow=arrow(), size = 1)+
  labs(y=expression(frac(E[2]-E[3],2)), x =expression(E[1]-frac(E[2]+E[3],2)))

library(gridExtra)

grid.arrange(p1,p2, ncol = 2)
ggsave("rotate.pdf")


###############################################
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
  p.x <- diff(pnorm(x.bin.edges, x, sigma*sqrt(3/2))) # sqrt(3/2) and sqrt(1/2) from covariance matrix
  p.y <- diff(pnorm(y.bin.edges, y, sigma*sqrt(1/2)))
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
attention <- c(1,1,2,2,2,3,3,3)
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

p2 <- ggplot(probcurves, aes(f,prob, group = finished, col = finished)) +geom_point() + geom_line(size = 1)+
  scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2")) +
  labs(colour = "Finished", y = "Probability", x = "Time step") + 
  theme(legend.position = "bottom", text = element_text(size = 20))+ theme_bw()

p1 <- ggplot(to.plot[f > 0,], aes(x, y)) + geom_tile(aes( fill=prob),alpha = 0.5) + 
  geom_point(aes(col = finished), alpha = 0.2) + theme_bw() + 
  facet_wrap(~ f, ncol = 4) +
  scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2"))+ 
  scale_fill_gradient(high = "darkorange1", low = "midnightblue")+ 
  labs(fill = "Probability", colour = "Finished",
       y=expression(frac(E[2]-E[3],2)), x =expression(E[1]-frac(E[2]+E[3],2)))+
  theme(text = element_text(size = 20))
p1 <- ggplot(to.plot[f > 0,], aes(x, y)) + geom_tile(aes( fill=prob),alpha = 0.5) + 
  geom_point(aes(col = finished), alpha = 0.3) + theme_bw() + 
  facet_wrap(~ f, ncol = 4) +
  scale_colour_manual(values = c("black", "sienna1","firebrick","dodgerblue2"))+ 
  scale_fill_gradient(high = "yellow", low = "navyblue")+ 
  labs(fill = "Probability", colour = "Finished", y= "", x = "")+
  theme(text = element_text(size = 20))+ theme_bw()+ guides(colour=FALSE)



###################with simulations
source("Rewritten.R")

simulate.rewritten.ADDM(item1 = 1, item2 = 6, item3 = 3, sigma=0.2, theta.unattended=0.3,
                        speed.of.integration=0.055, fix1=c(1,1,1,2,2,3), fix2=c(1,1,1,2,2,3),
                        prob = c(1,1,1), chosen = 3, no.simulations=100000, seed = c(1,2,3,4))

log.likelihood <- function(p, data) {
  theta <- p[1]
  sigma <- p[2]
  soi <- p[3]
  type <- p[4]
  no.of.sim <- 100000
  if (type == 1) {
    sum(log(mapply(simulate.rewritten.ADDM,item1 = as.vector(data$Item1_rating/7), item2 = as.vector(data$Item2_rating/7), item3 = as.vector(data$Item3_rating/7), fix1=as.vector(data$fix1), fix2=as.vector(data$fix2), prob = as.vector(data$prob), chosen = data$Chosen, no.simulations=no.of.sim, MoreArgs=list(sigma = sigma, speed.of.integration = soi,theta.unattended = theta, seed = seedno))))
  } else if  (type == 2) {
    sum(log(mapply(simulate.rewritten.ADDM,item1 = as.vector(data$Item1_max), item2 = as.vector(data$Item2_max), item3 = as.vector(data$Item3_max), fix1=as.vector(data$fix1), fix2=as.vector(data$fix2), prob = as.vector(data$prob), chosen = data$Chosen, no.simulations=no.of.sim,  MoreArgs=list(sigma = sigma, speed.of.integration = soi,theta.unattended = theta, seed = seedno))))
  } else if  (type == 3) {
    sum(log(mapply(simulate.rewritten.ADDM,item1 = as.vector(data$Item1_range), item2 = as.vector(data$Item2_range), item3 = as.vector(data$Item3_range), fix1=as.vector(data$fix1), fix2=as.vector(data$fix2), prob = as.vector(data$prob), chosen = data$Chosen, no.simulations=no.of.sim, MoreArgs=list(sigma = sigma, speed.of.integration = soi,theta.unattended = theta, seed = seedno))))
  } else {
    sum(log(mapply(simulate.rewritten.ADDM,item1 = as.vector(data$Item1_rank), item2 = as.vector(data$Item2_rank), item3 = as.vector(data$Item3_rank), fix1=as.vector(data$fix1), fix2=as.vector(data$fix2), prob = as.vector(data$prob), chosen = data$Chosen, no.simulations=no.of.sim, MoreArgs=list(sigma = sigma, speed.of.integration = soi,theta.unattended = theta, seed = seedno))))
  }
}

grid.arrange(p1,p2, ncol = 1) 

ggsave("process.pdf")