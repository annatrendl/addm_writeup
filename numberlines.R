rm(list=ls())
library(data.table)
library(ggplot2)


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
dataset[, Critical := rep(c("Adding a constant", "Multiplying by a constant", "Distant vs close second", "Distant vs close third"),each = 8)]



dataset <- melt(dataset, measure.vars = c("item1", "item2", "item3"),
             variable.name = "item", value.name = "value")
dataset[,phonyy := rep(c(1,1,1,1,1.05,1.05,1.05,1.05), 12)]


ggplot(dataset[Critical == "Adding a constant",],aes(x = value, y = phonyy, group =valueset, colour = valueset)) + geom_point() +
  ylim(c(0.95,1.1)) + facet_wrap(~ Transformation, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_x_continuous(breaks = c(0, 0.5, 1)) + labs(title = "Adding a constant")+ 
   scale_colour_manual(values = c("darkorange2", "mediumpurple4")) + theme(plot.title =element_text(size=13,hjust=0.5,face="bold")) +
  theme(legend.title = element_blank(),legend.text=element_text(size=12), strip.text = element_text(size=12))

ggsave("expl_add.jpg", width = 2.5, height =5.5)

ggplot(dataset[Critical == "Multiplying by a constant",],aes(x = value, y = phonyy, group =valueset, colour = valueset)) + geom_point() +
  ylim(c(0.95,1.1)) + facet_wrap(~ Transformation, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_x_continuous(breaks = c(0, 0.5, 1)) + labs(title = "Multiplying by a constant")+ 
  scale_colour_manual(values = c("darkorange2", "mediumpurple4")) + theme(plot.title =element_text(size=13,hjust=0.5,face="bold")) +
  theme(legend.title = element_blank(),legend.text=element_text(size=12), strip.text = element_text(size=12))

ggsave("expl_multipl.jpg", width = 2.5, height =5.5)

ggplot(dataset[Critical == "Distant vs close second",],aes(x = value, y = phonyy, group =valueset, colour = valueset)) + geom_point() +
  ylim(c(0.95,1.1)) + facet_wrap(~ Transformation, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_x_continuous(breaks = c(0, 0.5, 1)) + labs(title = "Distant vs close second")+ 
  scale_colour_manual(values = c("darkorange2", "mediumpurple4")) + theme(plot.title =element_text(size=13,hjust=0.5,face="bold")) +
  theme(legend.title = element_blank(),legend.text=element_text(size=12), strip.text = element_text(size=12))

ggsave("expl_second.jpg", width = 2.5, height =5.5)

ggplot(dataset[Critical == "Distant vs close third",],aes(x = value, y = phonyy, group =valueset, colour = valueset)) + geom_point() +
  ylim(c(0.95,1.1)) + facet_wrap(~ Transformation, ncol = 1) + theme(legend.position="bottom") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + scale_x_continuous(breaks = c(0, 0.5, 1)) + labs(title = "Distant vs close third")+ 
  scale_colour_manual(values = c("darkorange2", "mediumpurple4")) + theme(plot.title =element_text(size=13,hjust=0.5,face="bold")) +
  theme(legend.title = element_blank(),legend.text=element_text(size=12), strip.text = element_text(size=12))

ggsave("expl_third.jpg", width = 2.5, height =5.5)


