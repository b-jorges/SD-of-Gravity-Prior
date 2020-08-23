require(ggplot2)
require(gganimate)
require(dplyr)
require(cowplot)
theme_set(theme_cowplot())

# Where is this script?
Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}

#set working directory to where this file is
setwd(Where_Am_I())
source("Utilities/parabolic.r")
source("Utilities/functions.r")
source("Utilities/colourschemes.r")

RangeOfValues = seq(9.81-3,9.81+3,0.001)
Dataframe = data.frame(Gravity = rep(RangeOfValues,2),
                       Probability = c(dnorm(RangeOfValues,9.81,0.5),dnorm(RangeOfValues,9.81,1)),
                       TypeOfFunction = c(rep("Strong",length(RangeOfValues)),rep("Weak",length(RangeOfValues))))

ggplot(Dataframe %>% filter(TypeOfFunction == "Strong"),aes(Gravity,Probability)) +
  geom_line(size = 2) +
  geom_vline(xintercept = 9.81, size = 1, color = Red) +
  xlab("Gravity (m/s²)") +
  annotate(geom = "segment", x = 8.8, xend = 10.8, y = 0.1, yend = 0.1, color = BlauUB) +
  annotate(geom = "text", x = 12, y = 0.8, label = "Mean: ~9.81 m/s²", size = 6, color = Red) +
  annotate(geom = "text", x = 12, y = 0.1, label = "SD: ???", size = 6, color = BlauUB)

ggsave("Figure MeanSD Presentation 1.jpg", w = 4.5, h = 4.5)



Time = seq(0,1.5,0.01)
Dataframe = data.frame(Time2=c(Time,0.84), x = c(getXYpos(Time,vh = 6, vv = 6, g = 9.81)$x,-10), 
                       y = c(getXYpos(Time,vh = 6, vv = 6, g = 9.81)$y, -10)) %>% 
                       filter(x < 5)

plot = ggplot(Dataframe, aes(x,y)) +
  geom_point(size = 15) + 
  transition_time(Time2) +
  xlab("x (m)") +
  ylab("y (m)") +
  xlim(c(0,8)) +
  ylim(c(0,2)) +
  theme(text = element_text(size = 40),
        axis.text = element_text(size = 40))  
animate(plot, width = 1000, height = 1000, 
        nframes = 50, 
        detail = 14, 
        duration = 1,
        renderer = av_renderer())
anim_save("Ball.mp4")

#############normal distribution for "perception is probabilistic"
RangeOfValues = seq(6-4,6+4,0.01)
Dataframe = data.frame(Speed = RangeOfValues,
                       Probability = pnorm(RangeOfValues,6,1))
Dataframe = Dataframe %>%
  mutate(Answer = as.numeric(rbernoulli(length(Probability),Probability)))

#############normal distribution for Weber Fraction to standard deviation
ggplot(Dataframe, aes(Speed,Probability)) +
  geom_line(size = 2) +
  geom_point(data = Dataframe, aes(Speed, Answer), alpha = 0.2) +
  xlab("Presented Speed (m/s)") +
  ylab("Probability to choose Test") +
  coord_cartesian(xlim = c(Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))]-3,
                           Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))]+3)) +
  geom_vline(linetype = 2, xintercept = Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))], color = "grey") +
  geom_vline(linetype = 1, xintercept = Dataframe$Speed[which.min(abs(Dataframe$Probability-0.75))], color = "grey") +
  geom_vline(linetype = 1, xintercept = Dataframe$Speed[which.min(abs(Dataframe$Probability-0.25))], color = "grey") +
  geom_hline(linetype = 2, yintercept = 0.5, color = "grey") +
  geom_hline(linetype = 1, yintercept = 0.25, color = "grey") +
  geom_hline(linetype = 1, yintercept = 0.75, color = "grey") +
  geom_segment(aes(x=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.25))],
                   y=0.25,xend=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))],
                   yend = 0.25),
               color = Yellow, 
               size = 2) +
  geom_segment(aes(x=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))],
                   y=0.75,
                   xend=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.75))],
                   yend = 0.75),
               color = Yellow, 
               size = 2) +
  geom_segment(aes(x=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))],
                   y=0,
                   xend=Dataframe$Speed[which.min(abs(Dataframe$Probability-0.5))],
                   yend = 0.5),
               color = Red, 
               size = 2) +
  annotate("text",x = 6.75, y = 0.5, label = "PSE", color = Red, size = 6) +
  annotate("text",x = 4.75, y = 0.85, label = "~Weber Fraction", color = Yellow, size = 6) +
  scale_x_continuous(breaks=c(4,
                              Dataframe$Speed[which.min(abs(Dataframe$Probability-0.25))],
                              6,
                              Dataframe$Speed[which.min(abs(Dataframe$Probability-0.75))],
                              8))

ggsave("WeberFractionToSD.jpg", w = 5, h = 5)
(6.67-6)/6 
1/6
