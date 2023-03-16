### R version 4.2.2 (2022-10-31) ###

# R Package
install.packages("dplyr") # data manipulation
install.packages('lme4') # linear model analysis
install.packages('lmerTest') # linear mixed model analysis
install.packages('multcomp') # post hoc multiple comparisons
install.packages("ggplot2") # plot
install.packages("ggpubr") # plot 2
install.packages("report") # analysis report
library(dplyr)
library(lme4)
library(lmerTest)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(report)


# To plot data : prepare function summarySE (mean, SE, etc)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
read.csv("C:\\Users\\RiyaBiswas\\OneDrive - Exsurgo Rehab Ltd\\Desktop\\R package\\NFB21.csv")

# Categorical variable as factor
NFB21$Instruction <- as.factor(NFB21$Instruction)
NFB21$Block <- as.factor(NFB21$Block)
NFB21$Learner <- as.factor(NFB21$Learner)

# Effect of neurofeedback training on high alpha amplitude
lm <- lmer(highAlpha ~ Instruction * Block + (1 | id), NFB21)
lm.1 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.1)

# Plot - Evolution of high alpha amplitude during neurofeedback training
dfHA <- summarySE(NFB21, measurevar="highAlpha", groupvars=c("Block","Instruction"))
ggplot(dfHA, aes(x = Block, y = highAlpha, group= Instruction)) +
  geom_line(aes(y = highAlpha - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = highAlpha + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = highAlpha - se, ymax = highAlpha + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  ylim(4.5, 5.7) +
  labs(x = "Neurofeedback training blocks", y = "High alpha amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

# Effect of NFB training on high alpha amplitude during resting state
lm <- lmer(highAlphabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Plot - Evolution of high alpha amplitude during resting state (variable Block must be a factor)
ggplot(NFB21, aes(x=Block, y=highAlphabaseline, fill=Instruction)) +
  geom_violin(position=position_dodge(1), trim = TRUE, alpha = 0.7) +
  geom_boxplot(width=0.1, position=position_dodge(1))+
  theme_linedraw()+
  theme_light() +
  ylim(0, 17) +
  labs(x = "Resting state EEG", y = "High alpha amplitude (μV)") + 
  scale_x_discrete(labels=c("1" = "Resting state 1", "2" = "Resting state 2"),
                   limits=c("1", "2")) 

# Frequency of strategies used during NFB on high alpha amplitude
# Calculate strategies frequency by Learner
df <- NFB21 %>%
  group_by(Learner, Block, Strategy) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) 
# Comparison of strategies frequency used by Learner
lm = lm(Freq ~ Strategy * Learner, df)
report(lm)
anova(lm)
# Interaction Strategy * Learner
df$Inter <- interaction(df$Learner, df$Strategy)
mod <- lm(Freq ~ Inter, df)
summary(glht(mod, linfct=mcp(Inter="Tukey")), test = adjusted("holm"))

# Plot - Dot chart of Strategies frequency used by Learner
df2 <- NFB21 %>%
  group_by(Learner, Strategy) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) 

ggdotchart(df2, x = "Strategy", y = "Freq", 
  group = "Learner", color = "Learner", size = 1, dot.size = 3,
  add = "segment", position = position_dodge(0.5),
  sorting = "descending") + 
  scale_color_brewer(palette = "Set2")

# Effect of Strategies on high alpha amplitude by Learner
lm <- lm(highAlpha ~ Strategy * Learner, NFB21)
lm.2 <- anova(lm)
report(lm.2)
# Interaction Strategy * Learner
NFB21$Inter <- interaction(NFB21$Learner, NFB21$Strategy)
mod <- lm(highAlpha ~ Inter, NFB21)
summary(glht(mod, linfct=mcp(Inter="Tukey")), test = adjusted("holm"))

# Box plot - Effect of strategies used during NFB on high alpha amplitude
ggplot(data=NFB21, aes(x=Strategy, y=highAlpha, fill = Learner)) +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot()+
  theme_linedraw()+
  theme_light() +
  ylim(1, 15) +
  labs(x = "Strategy", y = "High alpha amplitude (μV)")

# Blox plot - Significant difference between strategies
ggplot(subset(NFB21, Strategy %in% c("Cognitive", "Memories")), aes(x=Strategy, y=highAlpha, fill = Learner)) +
  scale_fill_brewer(palette = "Set2") +
  geom_boxplot()+
  theme_linedraw()+
  theme_light() +
  ylim(0, 15) +
  labs(x = "Strategy", y = "High alpha amplitude (μV)")

# Resting state high alpha amplitude predict high alpha slope
lm = lm(highAlphaslope ~ Instruction * highAlphabaseline, NFB21)
lm.2 <- anova(lm)
report(lm.2)

# Scatter plot - Resting-state high alpha amplitude predict high alpha slope 
ggplot(NFB21, aes(x=highAlphabaseline, y=highAlphaslope, shape=Instruction, color=Instruction)) +
  geom_point() +
  theme_linedraw()+
  theme_light() +
  xlim(2.5, 10.2) +
  labs(x = "Resting state high alpha amplitude", y = "High alpha slope") +
  geom_smooth(method=glm, aes(fill=Instruction))

# High alpha slope predicted by working memory performance
lm = lm(highAlphaslope ~ Instruction + blockspan + digitspan + N1 + N2 + N3, NFB21)
lm.2 <- anova(lm)
report(lm.2)

# High alpha slope predicted by pre-training self-reported questionnaire
lm = lm(highAlphaslope ~ Instruction + calm + energetic  + happy + relaxed + satisfied + receptive + focused, NFB21)
lm.2 <- anova(lm)
report(lm.2)

# Effect of neurofeedback training on other frequencies amplitude during training
# Delta 
lm = lmer(delta ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Theta 
lm = lmer(theta ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Low alpha 
lm = lmer(lowAlpha ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Beta 
lm = lmer(beta ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Gamma 
lm = lmer(gamma ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Effect of neurofeedback training on other frequencies amplitude during resting EEG
# Delta baseline
lm = lmer(deltabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Theta baseline
lm = lmer(thetabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Low alpha 
lm = lmer(lowAlphabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Beta 
lm = lmer(betabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Gamma 
lm = lmer(gammabaseline ~ Instruction * Block + (1 | id), NFB21)
lm.2 <- anova(lm, type = 3, ddf = "Kenward-Roger")
report(lm.2)

# Line plot - Effect of neurofeedback training on other frequencies amplitude during training. 
# Delta 
dfD <- summarySE(NFB21, measurevar="delta", groupvars=c("Block","Instruction"))
ggplot(dfD, aes(x = Block, y = delta, group= Instruction)) +
  geom_line(aes(y = delta - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = delta + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = delta - se, ymax = delta + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  labs(x = "Neurofeedback training blocks", y = "Delta amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

# Theta 
dfT <- summarySE(NFB21, measurevar="theta", groupvars=c("Block","Instruction"))
ggplot(dfT, aes(x = Block, y = theta, group= Instruction)) +
  geom_line(aes(y = theta - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = theta + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = theta - se, ymax = theta + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  labs(x = "Neurofeedback training blocks", y = "Theta amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

#rlang::last_error()
#rlang::last_trace()

# Low alpha 
dfLA <- summarySE(NFB21, measurevar="lowAlpha", groupvars=c("Block","Instruction"))
ggplot(dfLA, aes(x = Block, y = lowAlpha, group= Instruction)) +
  geom_line(aes(y = lowAlpha - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = lowAlpha + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = lowAlpha - se, ymax = lowAlpha + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  labs(x = "Neurofeedback training blocks", y = "Low alpha amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

# Beta
dfB <- summarySE(NFB21, measurevar="beta", groupvars=c("Block","Instruction"))
ggplot(dfB, aes(x = Block, y = beta, group= Instruction)) +
  geom_line(aes(y = beta - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = beta + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = beta - se, ymax = beta + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  labs(x = "Neurofeedback training blocks", y = "Beta amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

# Gamma
dfG <- summarySE(NFB21, measurevar="gamma", groupvars=c("Block","Instruction"))
ggplot(dfG, aes(x = Block, y = gamma, group= Instruction)) +
  geom_line(aes(y = gamma - se), colour = "grey50", alpha = 0.4) +
  geom_line(aes(y = gamma + se), colour = "grey50", alpha = 0.4) +
  geom_ribbon(aes(ymin = gamma - se, ymax = gamma + se, fill=Instruction), alpha = 0.3) +
  geom_line(aes(color=Instruction)) +
  geom_point(aes(color=Instruction)) +
  theme_linedraw()+
  theme_light() +
  labs(x = "Neurofeedback training blocks", y = "Gamma amplitude (μV)") +
  scale_x_discrete(labels=c("T01" = "1", "T02" = "2", "T03" = "3", "T04" = "4", "T05" = "5", "T06" = "6", "T07" = "7", "T08" = "8", "T09" = "9", "T10" = "10"))

