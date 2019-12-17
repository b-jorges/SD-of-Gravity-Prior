###Pull the whole repository
# Where is this script?
Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}

setwd(Where_Am_I())

libraries <- c("ggplot2","dplyr", "lme4", "tidyr", "data.table", "tidyverse", "cowplot", "binr")

lapply(libraries, function(x) {
  if(!require(x, character.only = T, quietly = T)) {
    install.packages(x)
    require(x, character.only = T)
    }
  }
)

#####Load Packages and Code
#####

source("Utilities/parabolic.r")
source("Utilities/functions.r")

theme_set(theme_cowplot())
set.seed(121)

ColorPalette <- colorRampPalette(c(BlauUB,Yellow))(6)



b_a1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Lisa_1.txt", header = TRUE)
b_a1$Condition <- "-1g"
b_a2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Lisa_2.txt", header = TRUE)
b_a2$Condition <- "Different g"
b_a3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Lisa_3.txt", header = TRUE)
b_a3$Condition <- "Different g"
b_a4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Lisa_4.txt", header = TRUE)
b_a4$Condition <- "Different g"
b_b1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristian_1.txt", header = TRUE)
b_b1$Condition <- "Different g"
b_b2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristian_2.txt", header = TRUE)
b_b2$Condition <- "Different g"
b_b3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristian_3.txt", header = TRUE)
b_b3$Condition <- "Different g"
b_b4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristian_4.txt", header = TRUE)
b_b4$Condition <- "-1g"
b_c1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Borja_1.txt", header = TRUE)
b_c1$Condition <- "-1g"
b_c2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Borja_2.txt", header = TRUE)
b_c2$Condition <- "Different g"
b_c3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Borja_3.txt", header = TRUE)
b_c3$Condition <- "Different g"
b_c4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Borja_4.txt", header = TRUE)
b_c4$Condition <- "Different g"
b_d1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Clara_1.txt", header = TRUE)
b_d1$Condition <- "Different g"
b_d2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Clara_2.txt", header = TRUE)
b_d2$Condition <- "Different g"
b_d3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Clara_3.txt", header = TRUE)
b_d3$Condition <- "Different g"
b_d4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Clara_4.txt", header = TRUE)
b_d4$Condition <- "-1g"
b_e1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristina_1.txt", header = TRUE)
b_e1$Condition <- "-1g"
b_e2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristina_2.txt", header = TRUE)
b_e2$Condition <- "Different g"
b_e3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristina_3.txt", header = TRUE)
b_e3$Condition <- "Different g"
b_e4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Cristina_4.txt", header = TRUE)
b_e4$Condition <- "Different g"
b_f1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Aniol_1.txt", header = TRUE)
b_f1$Condition <- "Different g"
b_f2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Aniol_2.txt", header = TRUE)
b_f2$Condition <- "Different g"
b_f3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Aniol_3.txt", header = TRUE)
b_f3$Condition <- "Different g"
b_f4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Aniol_4.txt", header = TRUE)
b_f4$Condition <- "-1g"
b_g1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Juan_1.txt", header = TRUE)
b_g1$Condition <- "-1g"
b_g2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Juan_2.txt", header = TRUE)
b_g2$Condition <- "Different g"
b_g3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Juan_3.txt", header = TRUE)
b_g3$Condition <- "Different g"
b_g4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Juan_4.txt", header = TRUE)
b_g4$Condition <- "Different g"
b_h1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Rodolfo_1.txt", header = TRUE)
b_h1$Condition <- "Different g"
b_h2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Rodolfo_2.txt", header = TRUE)
b_h2$Condition <- "Different g"
b_h3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Rodolfo_3.txt", header = TRUE)
b_h3$Condition <- "Different g"
b_h4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Rodolfo_4.txt", header = TRUE)
b_h4$Condition <- "-1g"
b_i1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Marco_1.txt", header = TRUE)
b_i1$Condition <- "-1g"
b_i2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Marco_2.txt", header = TRUE)
b_i2$Condition <- "Different g"
b_i3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Marco_3.txt", header = TRUE)
b_i3$Condition <- "Different g"
b_i4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Marco_4.txt", header = TRUE)
b_i4$Condition <- "Different g"
b_j1 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Bjorn_1.txt", header = TRUE)
b_j1$Condition <- "Different g"
b_j2 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Bjorn_2.txt", header = TRUE)
b_j2$Condition <- "Different g"
b_j3 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Bjorn_3.txt", header = TRUE)
b_j3$Condition <- "Different g"
b_j4 <- read.table("(3) Eye-tracking and Gravity/experiment 3a/Response_Bjorn_4.txt", header = TRUE)
b_j4$Condition <- "-1g"



b_a1$block <- 1
b_a2$block <- 2
b_a3$block <- 3
b_a4$block <- 4
b_b1$block <- 1
b_b2$block <- 2
b_b3$block <- 3
b_b4$block <- 4
b_c1$block <- 1
b_c2$block <- 2
b_c3$block <- 3
b_c4$block <- 4
b_d1$block <- 1
b_d2$block <- 2
b_d3$block <- 3
b_d4$block <- 4
b_e1$block <- 1
b_e2$block <- 2
b_e3$block <- 3
b_e4$block <- 4
b_f1$block <- 1
b_f2$block <- 2
b_f3$block <- 3
b_f4$block <- 4
b_g1$block <- 1
b_g2$block <- 2
b_g3$block <- 3
b_g4$block <- 4
b_h1$block <- 1
b_h2$block <- 2
b_h3$block <- 3
b_h4$block <- 4
b_i1$block <- 1
b_i2$block <- 2
b_i3$block <- 3
b_i4$block <- 4
b_j1$block <- 1
b_j2$block <- 2
b_j3$block <- 3
b_f4$block <- 4

b_a1$id <- "s01"
b_a2$id <- "s01"
b_a3$id <- "s01"
b_a4$id <- "s01"

b_b1$id <- "s02"
b_b2$id <- "s02"
b_b3$id <- "s02"
b_b4$id <- "s02"

b_c1$id <- "s03"
b_c2$id <- "s03"
b_c3$id <- "s03"
b_c4$id <- "s03"

b_d1$id <- "s04"
b_d2$id <- "s04"
b_d3$id <- "s04"
b_d4$id <- "s04"

b_e1$id <- "s05"
b_e2$id <- "s05"
b_e3$id <- "s05"
b_e4$id <- "s05"

b_f1$id <- "s06"
b_f2$id <- "s06"
b_f3$id <- "s06"
b_f4$id <- "s06"
b_g1$id <- "s07"
b_g2$id <- "s07"
b_g3$id <- "s07"
b_g4$id <- "s07"
b_h1$id <- "s08"
b_h2$id <- "s08"
b_h3$id <- "s08"
b_h4$id <- "s08"
b_i1$id <- "s09"
b_i2$id <- "s09"
b_i3$id <- "s09"
b_i4$id <- "s09"
b_j1$id <- "s10"
b_j2$id <- "s10"
b_j3$id <- "s10"
b_j4$id <- "s10"

response_1 <- rbind(b_a1,b_a2,b_a3,b_a4)
response_2 <- rbind(b_b1,b_b2,b_b3,b_b4)
response_3 <- rbind(b_c1,b_c2,b_c3,b_c4)
response_4 <- rbind(b_d1,b_d2,b_d3,b_d4)
response_5 <- rbind(b_e1,b_e2,b_e3,b_e4)
response_6 <- rbind(b_f1,b_f2,b_f3,b_f4)
response_7 <- rbind(b_g1,b_g2,b_g3,b_g4)
response_8 <- rbind(b_h1,b_h2,b_h3,b_h4)
response_9 <- rbind(b_i1,b_i2,b_i3,b_i4)
response_10 <- rbind(b_j1,b_j2,b_j3,b_j4)

response <- rbind(response_1,response_2,response_3,response_4,response_5,response_6,response_7,response_8,response_9,
                  response_10)

remove(b_a1,b_a2,b_a3,b_a4,b_b1,b_b2,b_b3,b_b4,b_c1,b_c2,b_c3,b_c4,b_d1,b_d2,b_d3,b_d4,b_e1,b_e2,b_e3,b_e4,
       b_f1,b_f2,b_f3,b_f4,b_g1,b_g2,b_g3,b_g4,b_i1,b_i2,b_i3,b_i4,b_j1,b_j2,b_j3,b_j4,
       response_1,response_2,response_3,response_4,response_5,response_6,response_7,response_8,response_9,
       response_10)


response <- response[response$id != "s09" & abs(response$TemporalError) < 0.5,]
###Model responses, assuming that people assume 1g for all falling thingies

response <- response %>%
  ##Distance = (g/2)*t^2+vy*t
  ##0 = (g/2)*t^2+vy*t-Distance (Distance = Height of Disappearance)
  mutate(TemporalError = TemporalError-0.049259,
         PercentageOcclusion = OccludedTimeOfTrajectory/FlightDuration,
         TimeFallingBeforeOcclusion = (0.5-PercentageOcclusion)*FlightDuration,
         LastObserved_vy = g*TimeFallingBeforeOcclusion,
         MaxHeight = getYAtPeak(vy,g),
         HeightAtDisappearance = MaxHeight-TimeFallingBeforeOcclusion^2*(g/2),
         EstimatedTimeUnder1gAssumption = (-LastObserved_vy + (LastObserved_vy^2+4*(9.81/2)*
                                                                 (HeightAtDisappearance))^0.5)/(9.81),
         TemporalErrorUnder1gAssumption = EstimatedTimeUnder1gAssumption-OccludedTimeOfTrajectory,
         ControlTime = (TimeFallingBeforeOcclusion+OccludedTimeOfTrajectory)-FlightDuration/2,
         ControlTime2 = EstimatedTimeUnder1gAssumption - OccludedTimeOfTrajectory)


#####Coincidence Timing: Add categories, exclude data, ...
response$OcclusionCategory <- "long"
response$OcclusionCategory[response$OccludedTimeOfTrajectory < 0.25] <- "short"
response$g.factor <- as.factor(response$g)

response$g_2 <- "-1.0g" #this will be -1g
response$g_2[response$g == 9.81] <- "1.0g"
response$g_2[response$g == 6.8670] <- "0.7g"
response$g_2[response$g == 8.3385] <- "0.85g"
response$g_2[response$g == 11.2815] <- "1.15g"
response$g_2[response$g == 12.7530] <- "1.3g"

response <- response %>%
  group_by(id) %>%
  mutate(MeanTemporalErrorPerID = mean(TemporalError))

#####Compare medians and stuff
response <- response %>%
  group_by(g_2,Condition,LongOcclusion) %>%
  mutate(Median_Temporal_Error = median(TemporalError),
         Median_TemporalErrorUnder1gAssumption = median(TemporalErrorUnder1gAssumption)) ###This is the delay we measured in our system (SD = 0.001894s)


response <- response[!(response$id == "s09" & response$trial == 5 & response$block == 4), ] #exclude trials with odd behaviour due to system failure
response <- response[!(response$id == "s04" & response$trial == 1 & response$block == 3), ]
response <- response[!(response$id == "s10" & response$trial == 1 & response$block == 1), ]
response <- response[!(response$id == "s07" & response$trial == 1 & response$block == 3), ]

######Model prediction versus reality
response <- response %>%
  group_by(id, Condition,g,LongOcclusion) %>%
  mutate(Median_Error_Obs_ID = median(TemporalError),
         TemporalErrorUnder1gAssumption_ID = median(TemporalErrorUnder1gAssumption[abs(TemporalError) < 0.5]))
#exclude trials with odd behaviour due to system failure


response = response %>%
  group_by(Condition,id,vy,LongOcclusion, g) %>%
  mutate(MedianError = median(TemporalError),
         MeanError = mean(TemporalError),
         SDError = sd(TemporalError))

###########################What can the -1g/1g condition teach us?
ggplot(response[response$Condition == "-1g" & response$LongOcclusion == 1,], aes(x = as.factor(id), y = SDError, fill = as.factor(g_2))) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(vy~.) +
  scale_fill_manual(values=ColorPalette[c(1,4)]) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab("SD (s)")
ggsave("SD_Per_ID_-1g.jpg", w = 5, h = 5)

ggplot(response[response$Condition == "Different g" & response$LongOcclusion == 1,], aes(x = as.factor(id), y = SDError, fill = as.factor(g_2))) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(vy~.) +
  scale_fill_manual(values=ColorPalette[2:6]) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab("SD (s)")
ggsave("SD_Per_ID_DifferentG.jpg", w = 10, h = 5)


lala = response %>%
  arrange(Condition,vy,g) %>%
  filter(LongOcclusion == 1) %>%
  group_by(Condition,vy,g) %>%
  mutate(SD = sd(TemporalError),
         Mean = mean(TemporalError)) %>%
  slice(1) %>%
  select(Condition,vy,g,SD,Mean)
  
(lala$SD)  

response$CenteredError = response$TemporalError-response$MedianError
response$AbsError = abs(response$CenteredError)

mod1 = lmer(
  AbsError ~ as.factor(g) + ( 1 | id),
             data = response[response$Condition == "-1g" & response$LongOcclusion == 1,])
mod2 = lmer(
  AbsError ~ ( 1 | id),
  data = response[response$Condition == "-1g" & response$LongOcclusion == 1,])

anova(mod1,mod2)
summary(mod1)
coef(mod1)

haha = drawBayGraphs(meanLH = 0, sdLH = 0.75, meanP = 1, sdP = 1, title = "Normal Prior") +
  theme(legend.position = "") +
  theme(plot.title = element_text(hjust = 0.5))
haha2 = drawBayGraphs(meanLH = 0, sdLH = 0.75, meanP = 1, sdP = 0.4, title = "Strong Prior") +
  theme(plot.title = element_text(hjust = 0.5))
plot_grid(haha,haha2, nrow = 1, rel_widths = c(0.7,1))
ggsave("NormalPriorStrongPrior.jpg",w=10, h= 2.5)

response = response %>%
  group_by(id,vy,Condition,LongOcclusion) %>%
  mutate(SD_Ratio = sd(TemporalError[g == 9.81])/sd(TemporalError[g == -9.81]))

SD_Ratios = response %>%
  select(id,vy,LongOcclusion,id,SD_Ratio) %>%
  distinct()

SD_Ratios = SD_Ratios[complete.cases(SD_Ratios),]
mean(SD_Ratios$SD_Ratio)

####What to do with this? There is an added 12% benefit in terms of precision for gravity concordant motion.
####That doesnt seem like an awful lot. Maybe it is partially activated?

####Can the lack of error for -1g give an indication of how much* the gravity model was activated?
response = response %>%
  mutate(G_Represented = ((abs(HeightAtDisappearance)-abs(LastObserved_vy)*(OccludedTimeOfTrajectory+TemporalError))*2)/
           (OccludedTimeOfTrajectory+TemporalError)^2) %>%
  group_by(Condition,id,LongOcclusion,vy) %>%
  mutate(Ratio_Accuracy = mean(G_Represented[g == -9.81 & G_Represented < 100])/mean(G_Represented[g == 9.81 &  G_Represented < 100]))

response$G_Represented

response = response %>%
  group_by(Condition,id,g,LongOcclusion,vy) %>%
  mutate(SD_Accuracy = sd(G_Represented[G_Represented < 100]))

Ratios = response %>%
  select(id,Condition,vy,LongOcclusion,id,SD_Ratio,Ratio_Accuracy) %>%
  distinct()

ggplot(response[response$Condition == "-1g" & response$g == -9.81,], aes(x = as.factor(vy), y = TemporalError, col = as.factor(LongOcclusion))) +
         geom_violin()

Ratios = Ratios[complete.cases(Ratios),]
1-mean(Ratios$SD_Ratio[Ratios$LongOcclusion == 1]) ###percentage of precision loss for -1g versus 1g
1-mean(Ratios$Ratio_Accuracy[Ratios$LongOcclusion == 1 & Ratios$vy == 4.5])
1-mean(Ratios$Ratio_Accuracy[Ratios$LongOcclusion == 0 & Ratios$vy == 4.5]) ###percentage of accuracy loss for -1g versus 1g
1-mean(Ratios$Ratio_Accuracy[Ratios$LongOcclusion == 1 & Ratios$vy == 6])
1-mean(Ratios$Ratio_Accuracy[Ratios$LongOcclusion == 0  & Ratios$vy == 6]) ###percentage of accuracy loss for -1g versus 1g

ggplot(response[response$LongOcclusion == 1 & response$Condition == "Different g",], 
                         aes(g_2,TemporalError,fill = g_2)) + 
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_boxplot(width=0.1) +
  coord_cartesian(ylim = c(-0.5,+0.4)) +
  xlab("Gravity (g)") +
  ylab("Temporal Error (s)") +
  theme(legend.position = "") +
  scale_fill_manual(values = c(a[1],a[2],a[3],a[4],a[5])) +
  facet_grid(.~vy)

ggsave("TimingErrorObserved.jpg", w=10, h =5)

c1 <- lmer(
  formula = TemporalError ~ g + (1 | id),
  data = response[response$Condition == "Different g" & response$LongOcclusion == 1,]
)
c2 <- lmer(
  formula = TemporalError ~  (1 | id),
  data = response[response$Condition == "Different g" & response$LongOcclusion == 1,]
)

anova(c1,c2)
summary(c1)

response = response %>%
  group_by(id,LongOcclusion,Condition,vy,g) %>%
  mutate(PrecisionProxy = abs(TemporalError - median(TemporalError)))

d1 <- lmer(
  formula = PrecisionProxy ~ g + (1 | id),
  data = response[response$Condition == "Different g" & response$LongOcclusion == 1,]
)
d2 <- lmer(
  formula = PrecisionProxy ~  (1 | id),
  data = response[response$Condition == "Different g" & response$LongOcclusion == 1,]
)

anova(d1,d2)
summary(d1)


#####BALUPP! I could model the upwards with threshold for gravities/accelerations. 
#####Then I can estimate the SD of the response and stuff from that because I "know" all parameters
#####Then I can add that value to my other modelling stuff AND GET THE gravity SD!

#####Weber fractions of maybe like 20% for arbitrary gravities and accelerations (our 2018 paper, + 1/2 because we maybe used temporal information)
pnorm(1.15,1,0.222)
#an SD of 0.3, more or less
MotorNoiseSD = 0.05
n_Iterations = 500

GetSDMatchForMotorNoise = function(MotorNoiseSD,n_Iterations){
  b = c()
  for (i in 1:n_Iterations){
    response = response %>%
      mutate(SD_Factor_G = abs(rnorm(length(g),1,0.222)),
             SD_Factor_VY = abs(rnorm(length(g),1,0.148)),
             SD_Factor_Distance = abs(rnorm(length(g),1,0.15)),
             Response_Variability = abs(rnorm(length(g),0,MotorNoiseSD)),
             Mean_G = case_when(
               LongOcclusion == 1 & vy == 4.5 ~ 0.82*9.81,
               LongOcclusion == 0 & vy == 4.5 ~ 0.42*9.81,
               LongOcclusion == 1 & vy == 6 ~ 0.92*9.81,
               LongOcclusion == 0 & vy == 6 ~ 0.61*9.81
             ),
             Perceived_G = Mean_G*SD_Factor_G,
             Perceived_VY = abs(LastObserved_vy)*SD_Factor_VY,
             Perceived_Distance = abs(HeightAtDisappearance)*SD_Factor_Distance)
    
    response = response %>%
      mutate(TemporalEstimateWithUncertainty = (-Perceived_VY +
                                                  (Perceived_VY^2 +
                                                     2*Perceived_G*Perceived_Distance)^0.5)/
               Perceived_G,
             TemporalEstimateWithUncertainty_AndResponseSD = TemporalEstimateWithUncertainty + Response_Variability)
    
    ggplot(response[response$Condition == "-1g" & response$LongOcclusion == 1 ,], aes(as.factor(g),TemporalEstimateWithUncertainty_AndResponseSD-OccludedTimeOfTrajectory)) +
      geom_violin()
    
    #Here I get the SD of the participants timing - modelled responses and get actual responses
    response = response %>%
      group_by(g,vy,LongOcclusion,Condition) %>%
      mutate(SD_per_TTC_Modelled = sd(TemporalEstimateWithUncertainty_AndResponseSD-OccludedTimeOfTrajectory,na.rm = TRUE),
             SD_per_TTC_Real = sd(TemporalError,na.rm = TRUE),
             Error_Per_TTC = (SD_per_TTC_Real-SD_per_TTC_Modelled)^2)
    
    a = unique(response$Error_Per_TTC[response$Condition == "-1g"  & response$g == -9.81])
    b = c(b,mean(a))
    
    
    if (i==n_Iterations){
      print("Round done")
      print(MotorNoiseSD)
      print(mean(b))}
  }
  mean(b)
}

#####get general idea of range of values
Tentative_SD_Motor = seq(0,0.2,0.01)
error = c()
for (i in 1:length(Tentative_SD_Motor)){
  f = GetSDMatchForMotorNoise(Tentative_SD_Motor[i],500)
  error = c(error,f)
}
plot(Tentative_SD_Motor,error)

#Optimize over this function to get best SD fit for g
Optimization = optimize(GetSDMatchForMotorNoise, c(0.08,0.16), n_Iterations = 2000, maximum = FALSE, lower = 0.08, upper = 0.16, tol = 0.001)
Optimization


#################
######Lets get the SD of the gravity prior!
################
######We have the standard deviations and means for
######    timing (mean = 0, SD from data, SD = 0.11s)
######    distance to travel (estimate!)
######    velocity at disappearance (estimate!)#

######And want the SD of G
######We assume that everything is estimated accurately

#####How do Weber fractions translate into SDs again? STUFF I SHOULD KNOW! #ups
#This is the time modelled with uncertainty (GET MORE ACCURATE SDs!!! 
#How to translate weber fractions from percentage into SDs?) 
#Answer: its the SD of that normal distribution which gives 0.75 for mean +/- Weber Fraction 
#probably have to repeat this like a 1000 times?
#there are different sources of variability: not only g, but also the last seen velocity, 
#and the observed distance, BUT ALSO MAYBE FROM THE RESPONSE. What about the "integration" mechanism????
#also, we might wanna limit this analysis to the long trials????

#Weber fractions for velocities are like 5%, but its harder to extract the vertical velocity component from parabolic motion, so lets go with 10%?
#https://www.sciencedirect.com/science/article/pii/004269898190095X
#... weber fraction of 10% corresponds to:
pnorm(1.1,1,0.148)
#an SD of 0.148, more or less

#for lengths it's: https://pdfs.semanticscholar.org/c529/aa57cc49bb0fee25f695869312a876b3ca47.pdf 
#3-5% in fronto-parallel plane. However, they have pretty good reference and stuff, which are not present in our experiment
#in harder conditions, it's up to ~22%
#probably hard to find papers that cover exactly this ... lets go with 20%. Weber fraction of 20% corresponds to:
pnorm(1.05,1,0.15)
#an SD of about 0.15
n_Iterations = 2
SD_Gravity = 0.2

#motor error SD maybe around 30ms? https://jshd.pubs.asha.org/doi/full/10.1044/1092-4388%282003/012%29?casa_token=UAw_ubXyPlkAAAAA%3A9pfFHwevN336lmB1coLASAPwUqNFwn2W6Tes53k6JTM18xHlHnj4tGZNcF77Hwr_xsXCi3Vynxay&
#could be much lower, 10ms here: http://www.haskins.yale.edu/Reprints/HL1268.pdf
#interesting: motor error should be independent of length of prediction, f. e., while perceptual errors should increase with increased time window of prediction.


GetSDMatchForG = function(SD_Gravity,n_Iterations){
  b = c()
  for (i in 1:n_Iterations){
    response = response %>%
      mutate(SD_Factor_G = abs(rnorm(length(g),1,SD_Gravity)),
             SD_Factor_VY = abs(rnorm(length(g),1,0.148)),
             SD_Factor_Distance = abs(rnorm(length(g),1,0.15)),
             Response_Variability = abs(rnorm(length(g),0,0.16)),
             Perceived_G = 9.81*SD_Factor_G,
             Perceived_VY = LastObserved_vy*SD_Factor_VY,
             Perceived_Distance = HeightAtDisappearance*SD_Factor_Distance)
    
    response = response %>%
      mutate(TemporalEstimateWithUncertainty = (-Perceived_VY +
                                                  (Perceived_VY^2 +
                                                     2*Perceived_G*Perceived_Distance)^0.5)/
               Perceived_G,
             TemporalEstimateWithUncertainty_AndResponseSD = TemporalEstimateWithUncertainty + Response_Variability)
    
    #Here I get the SD of the participants timing - modelled responses and get actual responses
    response = response %>%
      group_by(g,vy,LongOcclusion,Condition) %>%
      mutate(SD_per_TTC_Modelled = sd(TemporalEstimateWithUncertainty_AndResponseSD-OccludedTimeOfTrajectory,na.rm = TRUE),
             SD_per_TTC_Real = sd(TemporalError,na.rm = TRUE),
             Error_Per_TTC = (SD_per_TTC_Real-SD_per_TTC_Modelled)^2)
    
    a = unique(response$Error_Per_TTC[response$LongOcclusion == 1 & response$Condition != "-1g"])
    b = c(b,mean(a))
    
    
    if (i==n_Iterations){
      print("Round done")
      print(SD_Gravity)
      print(mean(b))
    }
  }
  mean(b)
}

#Optimize over this function to get best SD fit for g
Optimization = optimize(GetSDMatchForG, c(0.18,0.22), n_Iterations = 500, maximum = FALSE, lower = 0.18, upper = 0.22, tol = 0.01)
Optimization


#####Check a range of gravity SD values
Tentative_SD_Gravity = seq(0.18,0.22,0.02)
error = c()
for (i in 1:length(Tentative_SD_Gravity)){
  f = GetSDMatchForG(Tentative_SD_Gravity[i],100)
  error = c(error,f)
}
plot(Tentative_SD_Gravity,error)

#####See with how many iterations the values stabilize
Number_Of_Iterations = c(seq(1,20,1),seq(21,50,2),seq(51,150,5),seq(151,300,10), seq(301,500, 20))
value = c()
for (i in 1:length(Number_Of_Iterations)){
  f = GetSDMatchForG(0.1,Number_Of_Iterations[i])
  value = c(value,f)
}
plot(Number_Of_Iterations,value)

