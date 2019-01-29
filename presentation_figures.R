##########################################################
library(Rmisc)
library(DescTools)
library(ggplot2)
library(gridExtra)
library(data.table)
library(lattice)
library(grid)

rm(list = ls())
source("simulations.R")
setwd("C:/Anna/addm_writeup")
load("merged_and_cleaned.RData")
load("exp2_choiceRT.RData")
complete_fixations[, no := as.numeric(paste0(sort(c(Item1_rating, Item2_rating, Item3_rating)),collapse = "")), by = 1:nrow(complete_fixations)]
complete_fixations[, diff12 := sort(c(Item1_rating, Item2_rating, Item3_rating),decreasing = TRUE)[1] - 
                     sort(c(Item1_rating, Item2_rating, Item3_rating),decreasing = TRUE)[2], by = 1:nrow(complete_fixations)]
complete_fixations[, diff23 := sort(c(Item1_rating, Item2_rating, Item3_rating),decreasing = TRUE)[2] - 
                     sort(c(Item1_rating, Item2_rating, Item3_rating),decreasing = TRUE)[3], by = 1:nrow(complete_fixations)]



Subj <- function(obj) {
  range <- sapply(obj, function(x) (x-min(obj))/(max(obj)-min(obj)))
  max <- sapply(obj, function(x) x/max(obj))
  rank <- sapply(obj, function(x) ifelse(x == max(obj),1,ifelse(x == min(obj),0,0.5)))
 # rank <- sapply(obj, function(x) x/sum(obj))
  # range_globmax <- sapply(obj, function(x) (x-min(obj))/(7-min(obj)))
  obj <- sapply(obj, function(x) x/7)
  cbind(obj,range,max,rank)
}

generate.value.plots <- function(item1, item2, item3, no.sim, original_dataset) {
  
   item1 <- c(1,5)
   item2 <- c(6,6)
   item3 <- c(7,7)
   no.sim <- 100000
   if  (all(diff(item1)==3, diff(item2)==3,diff(item3)==3)) {
     first <- 203050
     second <- 506080
   } else {
     first <- as.numeric(paste(c(item1[1], item2[1], item3[1])*10 + 10,collapse=""))
     second <- as.numeric(paste(c(item1[2], item2[2], item3[2])*10 + 10,collapse=""))
   }
   #get original
  original_dataset <- complete_fixations
  current <- original_dataset[no == as.numeric(paste0(sort(c(item1[1],item2[1],item3[1])),collapse = "")) |
                                no == as.numeric(paste0(sort(c(item1[2],item2[2],item3[2])),collapse = "")), ]
  current <- current[,list(count = .N), by = list(Which.chosen, no)]

  
  current[Which.chosen == "worst", Which.chosen := "Worst"]
  current[Which.chosen == "middle", Which.chosen := "Middle"]
  current[Which.chosen == "best", Which.chosen := "Best"]
  
  
  
  counts <- all_data[valueset == first | valueset == second,.N,.(valueset, which.chosen)]
  counts[which.chosen == "worst", which.chosen := "Worst"]
  counts[which.chosen == "middle", which.chosen := "Middle"]
  counts[which.chosen == "best", which.chosen := "Best"]
  
  counts[,which.chosen := factor(which.chosen, levels = c("Worst", "Middle", "Best"))] 

  
  counts <- counts[order(valueset)]
  
  counts <- cbind(counts, rbind(MultinomCI(counts[1:3,N], conf.level=0.95, method="sisonglaz"),
                                MultinomCI(counts[4:6,N], conf.level=0.95, method="sisonglaz")))

  current <- data.table(merge(expand.grid(Which.chosen = c("Best", "Middle", "Worst"), no = unique(current$no)),
                   current, by = c("Which.chosen", "no"), all.x = TRUE))
  current[is.na(count), count := 0]
  current[,Which.chosen := factor(Which.chosen,levels=c("Worst", "Middle", "Best"))]
  current <- current[order(no)]
  current <- cbind(current,rbind(MultinomCI(current[1:3,count], conf.level=0.95, method="sisonglaz"),
                                 MultinomCI(current[4:6,count], conf.level=0.95, method="sisonglaz")))
  
  
  
  noise <- 0.076
  speed.of.int <- 0.012
  values <- data.table(rbind(t(Subj(c(item1[1],item2[1],item3[1]))),
                             t(Subj(c(item1[2],item2[2],item3[2])))))
  
  objdiv <- c(as.numeric(round(values[1,],2)),as.numeric(round(values[5,],2)))
  range <-  c(as.numeric(round(values[2,],2)),as.numeric(round(values[6,],2)))
  max <-  c(as.numeric(round(values[3,],2)),as.numeric(round(values[7,],2)))
  rank <-  c(as.numeric(round(values[4,],2)),as.numeric(round(values[8,],2)))
  #range_globmax <-  c(as.numeric(round(values[5,],2)),as.numeric(round(values[10,],2)))
  
  dataset_obj <- data.table(rbind(get.sim(item1_r = as.numeric(values[1,1]), item2_r = as.numeric(values[1,2]), item3_r =  as.numeric(values[1,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim), get.sim(item1_r = as.numeric(values[5,1]), item2_r = as.numeric(values[5,2]), item3_r =  as.numeric(values[5,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)), Sim = rep(c(paste0(item1[1], item2[1], item3[1]),paste0(item1[2], item2[2], item3[2])), each = no.sim))
  
  
  dataset_range <- data.table(rbind(get.sim(item1_r = as.numeric(values[2,1]), item2_r = as.numeric(values[2,2]), item3_r =  as.numeric(values[2,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim), get.sim(item1_r = as.numeric(values[6,1]), item2_r = as.numeric(values[6,2]), item3_r =  as.numeric(values[6,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)),Sim = rep(c(paste0(item1[1], item2[1], item3[1]),paste0(item1[2], item2[2], item3[2])), each = no.sim))
  
  dataset_max <- data.table(rbind(get.sim(item1_r = as.numeric(values[3,1]), item2_r = as.numeric(values[3,2]), item3_r =  as.numeric(values[3,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim), get.sim(item1_r = as.numeric(values[7,1]), item2_r = as.numeric(values[7,2]), item3_r =  as.numeric(values[7,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)), Sim = rep(c(paste0(item1[1], item2[1], item3[1]),paste0(item1[2], item2[2], item3[2])), each = no.sim))
  
  dataset_rank <- data.table(rbind(get.sim(item1_r = as.numeric(values[4,1]), item2_r = as.numeric(values[4,2]), item3_r =  as.numeric(values[4,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim), get.sim(item1_r = as.numeric(values[8,1]), item2_r = as.numeric(values[8,2]), item3_r =  as.numeric(values[8,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim)), Sim = rep(c(paste0(item1[1], item2[1], item3[1]),paste0(item1[2], item2[2], item3[2])), each = no.sim))
  
  #dataset_range_globmax <- data.table(rbind(get.sim(item1_r = as.numeric(values[5,1]), item2_r = as.numeric(values[5,2]), #item3_r =  as.numeric(values[5,3]), noise = noise, speed.of.int = speed.of.int, no.sim = no.sim), get.sim(item1_r = #as.numeric(values[10,1]), item2_r = as.numeric(values[10,2]), item3_r =  as.numeric(values[10,3]), noise = noise, speed.of.int #= speed.of.int, no.sim = no.sim)), Sim = rep(c(paste0(item1[1], item2[1], item3[1]),paste0(item1[2], item2[2], item3[2])), #each = no.sim))
  
  
  dataset_obj <- dataset_obj[, list(count = .N), by = list(Sim, Chosen)]
  dataset_obj <- dataset_obj[order(Sim)]
  dataset_obj <- cbind(dataset_obj,rbind(MultinomCI(dataset_obj[1:3,count], conf.level=0.95, method="sisonglaz"),
                                         MultinomCI(dataset_obj[4:6,count], conf.level=0.95, method="sisonglaz")))
  
  
  dataset_range <- dataset_range[, list(count = .N), by = list(Sim, Chosen)]
  dataset_range <- dataset_range[order(Sim)]
  dataset_range <- cbind(dataset_range,rbind(MultinomCI(dataset_range[1:3,count], conf.level=0.95, method="sisonglaz"),
                                             MultinomCI(dataset_range[4:6,count], conf.level=0.95, method="sisonglaz")))
  
  dataset_max <- dataset_max[, list(count = .N), by = list(Sim, Chosen)]
  dataset_max <- dataset_max[order(Sim)]
  dataset_max <- cbind(dataset_max,rbind(MultinomCI(dataset_max[1:3,count], conf.level=0.95, method="sisonglaz"),
                                         MultinomCI(dataset_max[4:6,count], conf.level=0.95, method="sisonglaz")))
  
  dataset_rank <- dataset_rank[, list(count = .N), by = list(Sim, Chosen)]
  dataset_rank <- dataset_rank[order(Sim)]
  dataset_rank <- cbind(dataset_rank,rbind(MultinomCI(dataset_rank[1:3,count], conf.level=0.95, method="sisonglaz"),
                                           MultinomCI(dataset_rank[4:6,count], conf.level=0.95, method="sisonglaz")))
  
  
  #dataset_range_globmax <- dataset_range_globmax[, list(count = .N), by = list(Sim, Chosen)]
  #dataset_range_globmax <- dataset_range_globmax[order(Sim)]
  #dataset_range_globmax <- cbind(dataset_range_globmax,rbind(MultinomCI(dataset_range_globmax[1:3,count], conf.level=0.95, method="sisonglaz"),
  #                               MultinomCI(dataset_range_globmax[4:6,count], conf.level=0.95, method="sisonglaz")))
  

  sizz <- 18
  
  
  plot1 <- ggplot(dataset_obj, aes(x= Chosen, y = est, fill=factor(Sim))) + 
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1)) + theme(legend.text=element_text(size=12)) +
    #  geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") + 
    labs(title=paste("Global Max,\n",objdiv[1], objdiv[2], objdiv[3], "\n",objdiv[4], objdiv[5], objdiv[6]),fill="") +
    theme(plot.title = element_text(size=13,hjust=0.5)) + theme(legend.position="bottom") + ylab(c(" ")) + 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("") +
      scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))+ theme(legend.text=element_text(size=19))
  
  plot4 <- ggplot(dataset_range, aes(x= Chosen, y = est, fill=factor(Sim))) + 
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) + 
    #   geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") + 
    labs(title=paste("Range,\n",range[1], range[2], range[3], "\n",range[4], range[5], range[6]),fill="")+
    theme(plot.title = element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))
  
  plot2 <- ggplot(dataset_max, aes(x= Chosen, y = est, fill=factor(Sim))) + 
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) +
    #   geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") +
    labs(title=paste("Local Max,\n",max[1], max[2], max[3], "\n",max[4], max[5], max[6]),fill="")+
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))
  
  plot3 <- ggplot(dataset_rank, aes(x= Chosen, y = est, fill=factor(Sim))) + 
    geom_bar(stat="identity", position="dodge") + ylim(c(0,1))  + theme(legend.text=element_text(size=12)) +
    #  geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") +
    labs(title=paste("Rank,\n",rank[1], rank[2], rank[3], "\n",rank[4], rank[5], rank[6]),fill="")+
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ 
    scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))
  
  
  #plot5 <- ggplot(dataset_range_globmax, aes(x= Chosen, y = est, fill=factor(Sim))) + 
  #geom_bar(stat="identity", position="dodge") + ylim(c(0,1)) + 
  #  geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = "dodge") +
  #  labs(title=paste("Range_globmax:",range_globmax[1], range_globmax[2], range_globmax[3], "vs",range_globmax[4], range_globmax[5], range_globmax[6]),fill="")+
  #  theme(plot.title = element_text(size=8))
  
  #plot1 <- ggplot(dataset_obj, aes(x= Chosen)) + geom_bar() +labs(title=paste("Objdiv:",objdiv[1], objdiv[2], objdiv[3])) + ylim(c(0,no.sim)) + theme(plot.title = element_text(size=10))
  #plot2 <- ggplot(dataset_range, aes(x= Chosen)) + geom_bar()+labs(title=paste("Range_globmax:",range[1], range[2], range[3]))+ ylim(c(0,no.sim))+ theme(plot.title = element_text(size=10))
  #plot3 <- ggplot(dataset_max, aes(x= Chosen)) + geom_bar()+labs(title=paste("Max:",max[1], max[2], max[3]))+ ylim(c(0,no.sim))+ theme(plot.title = element_text(size=10))
  #plot4 <- ggplot(dataset_rank, aes(x= Chosen)) + geom_bar()+labs(title=paste("Rank:",rank[1], rank[2], rank[3]))+ ylim(c(0,no.sim))+ theme(plot.title = element_text(size=10))
  
  #counts should be data from exp 2

 
  plot6 <- ggplot(counts, aes(x = which.chosen,y =est, fill=as.factor(valueset)))+
    geom_bar(stat="identity", position = "dodge") + geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = position_dodge(width = 0.9), width=0.2) +
    ylim(c(0,1)) + labs(title = paste("Data\nExperiment 2"), fill = "") + theme(legend.position="bottom")  + theme(legend.text=element_text(size=12)) +
    theme(plot.title =element_text(size=13,hjust=0.5))+ ylab(c(" "))+ scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))
  
  
  plot5 <- ggplot(current, aes(x=Which.chosen, y =est, fill = as.factor(no))) + geom_bar(stat = "identity", position = "dodge") +
    ylim(c(0,1)) + geom_errorbar(aes(ymax = upr.ci, ymin = lwr.ci), position = position_dodge(width = 0.9), width=0.2)  + labs(title = paste("Data\nExperiment 1"), fill = "") +
    theme(plot.title =element_text(size=13,hjust=0.5))+  theme(legend.position="bottom")  + theme(legend.text=element_text(size=12)) +
    ylab(c(" "))+ scale_x_discrete(labels=c("1" = "Worst", "2" = "Middle", "3" = "Best")) +xlab("")+
    scale_fill_manual(values=c("darkorange2", "mediumpurple4"))+
    theme(legend.text.align = 0)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
          plot.title = element_text(size=sizz))
  
  
  
  
  
  #plot5 <- ggplot(dataset_obj, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf()  +labs(title=paste("Obj div, values:",objdiv[1], objdiv[2], objdiv[3]))
  #plot6 <- ggplot(dataset_range, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf()+labs(title=paste("Range, values:",range[1], range[2], range[3]))
  #plot7 <-ggplot(dataset_max, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf() +labs(title=paste("Max, values:",max[1], max[2], max[3]))
  #plot8 <- ggplot(dataset_rank, aes(When, colour = Chosen, group = Chosen)) + stat_ecdf() +labs(title=paste("Rank, values:",rank[1], rank[2], rank[3]))
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(plot1)
  
  
  predictions <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
                                 plot2 + theme(legend.position="none"),
                                 plot5 + theme(legend.position="none"),
                                 plot3 + theme(legend.position="none"),
                                 plot4 + theme(legend.position="none"),
                                 plot6 + theme(legend.position="none"),
                              # top = textGrob("Predictions", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
                               left = textGrob("Choice Proportion", rot = 90, vjust = 1, gp = gpar(fontsize = 18)),
                               # 
                                 nrow=2),mylegend, heights=c(10, 1))
  
  # predictions <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
  #                                         plot2 + theme(legend.position="none"),
  #                                        # plot5 + theme(legend.position="none"),
  #                                         plot3 + theme(legend.position="none"),
  #                                         plot4 + theme(legend.position="none"),
  #                                       #  plot6 + theme(legend.position="none"),
  #                                         # top = textGrob("Predictions", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
  #                                         left = textGrob("choice proportion", rot = 90, vjust = 1),
  #                                         # 
  #                                         nrow=2),mylegend, heights=c(10, 1))
  # 
  
#  mylegend<-g_legend(plot5)
#  results <- grid.arrange(arrangeGrob(plot5 + theme(legend.position="none"),
#                                          plot6 + theme(legend.position="none"),
#                                          ncol=1),mylegend,
#                               ncol=1, heights=c(10, 1))
  
#results <- multiplot(plot5, plot6, cols=1)
  
  #print(multiplot(plot1, plot2, plot3, plot4, cols=2))
  #print(predictions)
  #print(results)
  #print(c(item1,item2,item3))
  pred <<- predictions
  res <<- results
    #print(multiplot(plot5, plot6, plot7, plot8,cols=2))
}


results <- vector(mode = "list")
results[[1]] <- generate.value.plots(c(1,4),c(2,5),c(4,7),100000, complete_fixations)
results[[2]] <- generate.value.plots(c(1,4),c(2,4),c(3,6),100000, complete_fixations)
results[[3]] <- generate.value.plots(c(1,4),c(3,6),c(7,7),100000, complete_fixations)
results[[4]] <- generate.value.plots(c(1,4),c(2,4),c(3,6),100000, complete_fixations)









