setwd("C:/Anna/addm_writeup")
library(data.table)
library(ggplot2)

set.seed(10)

n <- 1000
x <- cumsum(sample(c(-0.1, 0.1), n, TRUE))

data <- data.table("Time_step" = c(seq(1:1000)), "Relative_Evidence" = x) 

ggplot(data = data[Time_step <= 721], aes(x = Time_step, y = Relative_Evidence)) + geom_line() + 
  geom_hline(yintercept = 2.5, colour ="mediumpurple4") + 
  geom_hline(yintercept = -2.5, colour ="darkorange2") + xlim(c(0,850)) + ylim(c(-3,3)) +
  annotate("text", x = 200, y = 2.7,size = 5, label = "Decision threshold for choosing option A", colour = "mediumpurple4")+
  annotate("text", x = 200, y = -2.7,size = 5, label = "Decision threshold for choosing option B", colour = "darkorange2") +
  geom_vline(xintercept = 721, linetype = "dashed") + 
  geom_point(data= data[Time_step == 721,], colour = "navyblue", size = 3) +
  xlab("Time Step") + ylab("Relative Evidence Accumulated") +
  annotate("text", x = 800, y = 2.3, label = "Option A chosen", colour = "navyblue", size = 5) +
  theme(text = element_text(size = 20))
ggsave("c1_randomwalk.pdf")