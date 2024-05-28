library(tidyverse)
library(nlme)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(reshape2)
library(dplyr)
library(psych)
library(ggpubr)
library(ggpattern)

training_t2 <- read.csv("data/training_t2_CLEAN.csv")
control_t2 <- read.csv("data/control_t2_CLEAN.csv")
control_t1<- read.csv("data/control_t1_CLEAN.csv")
training_t1 <- read.csv("data/training_t1_CLEAN.csv")

#cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c( "#D55E0095", "#0072B295")

##checking the ids match for both groups 
training_t1$ids == training_t2$ids
control_t1$ids == control_t2$ids

#rm <- sample(training_t1$ids, 1) 
### initially used this to randomly choose a participant from 
## the training group to exclude to make the groups even sizes - 
## the initial exclusion was 9361 so it has been hardcoded 
###in for reproducibility 

training_t1[which(training_t1$ids == 9361),] <- NA
training_t2[which(training_t2$ids == 9361),] <- NA

training_t2 <- training_t2[complete.cases(training_t2),]
training_t1 <- training_t1[complete.cases(training_t1),]



training_t2$hr_diff <- training_t2$dungeon_HR - training_t1$baseline_HR
control_t2$hr_diff <- control_t2$dungeon_HR - control_t1$baseline_HR

training_t2$sdnn_diff <- training_t2$dungeon_sdnn - training_t1$baseline_sdnn
control_t2$sdnn_diff <- control_t2$dungeon_sdnn - control_t1$baseline_sdnn

options(scipen=999)

################################################
## T-tests 
################################################


t.test(training_t2$hr_diff, control_t2$hr_diff)
t.test(training_t2$sdnn_diff, control_t2$sdnn_diff)


################################################
##CLEANING 
################################################


## horror sanity check - there should not be a difference
## between sdnn and hr in the groups for horror 

#t.test(training_t1$horror_HR, control_t1$horror_HR)
#t.test(training_t1$horror_sdnn, control_t1$horror_sdnn)


################################################
### BL normalised HR repeated measures ANOVA ###
################################################


training_melt <- data.frame(ids = rep(training_t1$ids,2),
                            time = c(rep("intruder", 27), rep("stressor",27)),
                            HR = c(training_t1$horror_HR - training_t1$baseline_HR,
                                   training_t2$hr_diff),
                            group = rep("training", 54))

control_melt <- data.frame(ids = rep(control_t1$ids,2),
                           time = c(rep("intruder", 27), rep("stressor",27)),
                           HR = c(control_t1$horror_HR - control_t1$baseline_HR,
                                  control_t2$hr_diff),
                           group = rep("control", 54))


melt.aov <- rbind(control_melt, training_melt)

#rm(training_melt, control_melt)

melt.aov$time <- factor(melt.aov$time, levels=c("intruder", "stressor"))
melt.aov$group <- factor(melt.aov$group)


#test normality 
melt.aov %>%
  group_by(time, group) %>%
  shapiro_test(HR)

#ggqqplot(melt.aov, "HR", ggtheme = theme_bw()) +
#  facet_grid(time ~ group)

#test homogeneity of variance
melt.aov %>%
  group_by(time) %>%
  levene_test(HR ~ group)

#test homogeneity of covariance
box_m(melt.aov[, "HR", drop = FALSE], melt.aov$group)

# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = melt.aov, dv = HR, wid = ids,
  between = group, within = time
)
get_anova_table(res.aov)

# Effect of time at each level of group
one.way <- melt.aov %>%
  group_by(group) %>%
  anova_test(dv = HR, wid = ids, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "fdr")
one.way

##calculate the means for control and training

mean(control_t1$horror_HR)
mean(control_t2$dungeon_HR)


melt.aov %>% 
  group_by(group, time) %>% 
  summarise(hr_mean = mean(HR)) -> melt.aov2


melt.aov %>% 
  group_by(group, time) %>% 
  summarise(sd = sd(HR)) -> melt.aov3

melt.aov2$sd = melt.aov3$sd
melt.aov2$sd = melt.aov2$sd / sqrt(27)

rm(melt.aov3)


hr_aov_plot<- ggplot(data = melt.aov2) +
  aes(x = time, y = hr_mean, color = group) +
  geom_line(aes(group = group), linewidth = 1.2) +
  scale_color_manual(values=cbPalette)+
  labs(title = "",
       x = "Scenario",
       y = "HR ",
       fill = "group")+
  #ggtitle("CTQ Score")   +
  geom_point()+
  ggtitle("HR in Intruder and Stressor")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(margin=margin(0,0,13,0))) +
  theme(plot.title=element_text(face="bold")) +
  theme(plot.title=element_text(size=13)) +
  theme(axis.text = element_text(colour = "#00000095")) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text=element_text(size=11))+
  theme(axis.title.x = element_text(margin=margin(13,0,0,0))) +
  theme(axis.title.y = element_text(margin=margin(0,13,0,0))) +
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(1, 'cm'))+
  geom_errorbar(aes(ymin = hr_mean-sd, ymax= hr_mean+sd),width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(n.breaks=5) 

hr_aov_plot  + guides(colour=FALSE)


rm(melt.aov, melt.aov2, model.aov, res.aov, one.way)

###################################################
### BL normalised SDNN repeated measures ANOVA ###
##################################################

training_sdnn_melt <- data.frame(ids = rep(training_t1$ids,2),
                                 time = c(rep("intruder", 27), rep("stressor",27)),
                                 SDNN = c(training_t1$horror_sdnn - training_t1$baseline_sdnn,
                                          training_t2$sdnn_diff),
                                 group = rep("training", 54))

control_sdnn_melt <- data.frame(ids = rep(control_t1$ids,2),
                                time = c(rep("intruder", 27), rep("stressor",27)),
                                SDNN = c(control_t1$horror_sdnn - control_t1$baseline_sdnn,
                                         control_t2$sdnn_diff),
                                group = rep("control", 54))

melt.aov <- rbind(control_sdnn_melt, training_sdnn_melt)

rm(control_sdnn_melt, training_sdnn_melt)


melt.aov$time <- factor(melt.aov$time, levels=c("intruder", "stressor"))
melt.aov$group <- factor(melt.aov$group)


#test normality 
melt.aov %>%
  group_by(time, group) %>%
  shapiro_test(SDNN)

#ggqqplot(melt.aov, "SDNN", ggtheme = theme_bw()) +
 # facet_grid(time ~ group)

#test homogeneity of variance
melt.aov %>%
  group_by(time) %>%
  levene_test(SDNN ~ group)

#test homogeneity of covariance
box_m(melt.aov[, "SDNN", drop = FALSE], melt.aov$group)

# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = melt.aov, dv = SDNN, wid = ids,
  between = group, within = time
)
get_anova_table(res.aov)


# Pairwise comparisons between group levels
pwc <- melt.aov %>%
  group_by(time) %>%
  pairwise_t_test(SDNN ~ group, p.adjust.method = "bonferroni")
pwc

melt.aov <- melt.aov %>% 
  group_by(group) %>% 
  mutate(mean = mean(SDNN))

melt.aov <- melt.aov %>% 
  group_by(time) %>% 
  mutate(time_mean = mean(SDNN))

# Effect of time at each level of exercises group
one.way <- melt.aov %>%
  group_by(group) %>%
  anova_test(dv = SDNN, wid = ids, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "fdr")
one.way


# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- melt.aov %>%
  group_by(group) %>%
  pairwise_t_test(
    SDNN ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(, -p) # Remove details

print(pwc2)


bxp <- ggboxplot(
  melt.aov, x = "time", y = "SDNN",
  color = "group", palette= cbPalette,
  ylab = "SDNN", xlab = "Condition") +   
  theme_minimal()+
  theme(axis.text = element_text(colour = "#00000095")) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text=element_text(size=11))+
  theme(axis.title.x = element_text(margin=margin(13,0,0,0))) +
  theme(axis.title.y = element_text(margin=margin(0,13,0,0))) +
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(1, 'cm'))+
  scale_y_continuous(n.breaks=5) 

bxp



melt.aov %>% 
  group_by(group, time) %>% 
  summarise(sdnn_mean = mean(SDNN)) -> melt.aov2


melt.aov %>% 
  group_by(group, time) %>% 
  summarise(sd = sd(SDNN)) -> melt.aov3

melt.aov2$sd = melt.aov3$sd
melt.aov2$sd = melt.aov2$sd / sqrt(27)

rm(melt.aov3)



sdnn_aov_plot <- ggplot(data = melt.aov2) +
  aes(x = time, y = sdnn_mean, color = group) +
  geom_line(aes(group = group), linewidth = 1.2) +
  scale_color_manual(values=cbPalette)+
  labs(title = "",
       x = "Scenario",
       y = "cvSDNN ",
       fill = "group")+
  geom_point() +
  ggtitle("cvSDNN in Intruder and Stressor")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(margin=margin(0,0,13,0))) +
  theme(plot.title=element_text(face="bold")) +
  theme(plot.title=element_text(size=13)) +
  theme(axis.text = element_text(colour = "#00000095")) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text=element_text(size=11))+
  theme(axis.title.x = element_text(margin=margin(13,0,0,0))) +
  theme(axis.title.y = element_text(margin=margin(0,13,0,0))) +
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(1, 'cm'))+
  geom_errorbar(aes(ymin = sdnn_mean-sd, ymax= sdnn_mean+sd),width=.2,
                position=position_dodge(0.05)) +
  scale_y_continuous(n.breaks=5) 

sdnn_aov_plot + theme(legend.title = element_blank())



rm(melt.aov, melt.aov2, bxp, pwc, pwc2, one.way, res.aov)


###################################################
############## Respiration analysis ############## 
##################################################

controlResp <- read.csv("data/controlResp.csv")
trainResp <- read.csv("data/trainingResp.csv")

trainResp$diff <- trainResp$dungeonBreathTrain - trainResp$baselineBreathTrain
controlResp$contDiff <- controlResp$dungeonBreathCont - controlResp$baselineBreathCont


# rm <- sample(trainResp$ids, 4) 
# Initially I selected 4 random datasets from the training group to exclude
#these were 6782, 9361, 6924 and 4486
# for replicability I'm now manually removing those participants
# so we don't randomly remove different participants each time and alter results

trainResp[which(trainResp$ids == 6782),] <- NA
trainResp[which(trainResp$ids == 9361),] <- NA
trainResp[which(trainResp$ids == 6924),] <- NA
trainResp[which(trainResp$ids == 4486),] <- NA

trainResp <- trainResp[complete.cases(trainResp),]


mean(trainResp$diff)
mean(controlResp$contDiff)


t.test(controlResp$contDiff, trainResp$diff)


#### plotting difference in resp change
### between control and training groups 


colnames(trainResp)[2] <- "baseline" 
colnames(trainResp)[3] <- "stressor" 
colnames(controlResp)[2] <- "baseline" 
colnames(controlResp)[3] <- "stressor" 


diffs <- data.frame(control = controlResp$contDiff, 
                    training = trainResp$diff)


diffs <- melt(diffs)
diffs$variable <- factor(diffs$variable)


 
resp_box <- ggplot(data = diffs) +
  aes(data = variable, y = value, color = variable) +
  geom_boxplot(show.legend = FALSE, linewidth = 1) +
  theme_classic()+
  scale_color_manual(values=cbPalette)+
   labs(title = "Respiration Rate Change from Baseline to Stressor",
       x = "Group",
       y = "Respiration Rate (breaths/min) ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(margin=margin(0,0,13,0))) +
  theme(plot.title=element_text(face="bold")) +
  theme(plot.title=element_text(size=13)) +
  theme(axis.text = element_text(colour = "#00000095")) +
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text=element_text(size=11))+
  theme(axis.title.x = element_text(margin=margin(13,0,0,0))) +
  theme(axis.title.y = element_text(margin=margin(0,13,0,0))) +
  theme(legend.text = element_text(size=12))+
  theme(legend.key.size = unit(1, 'cm'))+
  scale_y_continuous(n.breaks=5) 

ggarrange( hr_aov_plot + theme(legend.title = element_blank()), sdnn_aov_plot + theme(legend.title = element_blank()),
         resp_box, common.legend = T, labels = 'AUTO', legend = "left", ncol =3)


### SDNN respiratory correlation 

controlSDNN <- control_t2[control_t2$ids %in% controlResp$ids,]
trainSDNN <- training_t2[training_t2$ids %in% trainResp$ids,]

trainResp <- trainResp[trainResp$ids %in%trainSDNN$ids,]
controlResp <- controlResp[controlResp$ids %in% controlSDNN$ids,]

resp_cor <- data.frame(SDNN_diff = c(controlSDNN$sdnn_diff, trainSDNN$sdnn_diff),
                       resp_diff = c(controlResp$contDiff,trainResp$diff))


library(ggpubr) 

corr.test(resp_cor$SDNN_diff, resp_cor$resp_diff)

ggscatter(resp_cor, x = 'SDNN_diff', y = 'resp_diff', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cvSDNN Difference", ylab = "Resp Difference")+
          ggtitle("Correlation between cvSDNN and Respiration Rate")+
          theme_minimal()+
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(plot.title = element_text(margin=margin(0,0,13,0))) +
          theme(plot.title=element_text(face="bold")) +
          theme(plot.title=element_text(size=13)) +
          theme(axis.text = element_text(colour = "#00000095")) +
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold")) +
          theme(axis.title.x = element_text(size = 12)) +
          theme(axis.title.y = element_text(size = 12)) +
          theme(axis.text=element_text(size=11))+
          theme(axis.title.x = element_text(margin=margin(13,0,0,0))) +
          theme(axis.title.y = element_text(margin=margin(0,13,0,0))) +
          theme(legend.text = element_text(size=12))+
          theme(legend.key.size = unit(1, 'cm'))+
          scale_y_continuous(n.breaks=5)
