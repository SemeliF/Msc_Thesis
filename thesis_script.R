
# CODE USED FOR MASTER THESIS
# MARIA SEMELI FRANGOPOULOU - 2020452
#------------------------------------------------------------------------------

# INSTALLING PACKAGES

install.packages("haven") #load data
install.packages("robustbase")
install.packages("GGally")
install.packages("stringr")
install.packages("psych")
install.packages("ggcorrplot")
install.packages("car")
install.packages("rstudioapi") #path specification
install.packages("interactions") #may not have been used after all
install.packages("emmeans") #EMMs
install.packages("Hmisc") #correlation matrix
install.packages("corrplot") #correlation figure
install.packages("sjlabelled") #change labels 

library(corrplot)
library("Hmisc")
library(emmeans)
library("interactions")
library(rstudioapi)
library(ggcorrplot)
library(car) #for VIF
library(stringr)
library("haven")
library("readr")
library("dplyr")
library("robustbase")
library("GGally")
library(psych)
library(ggplot2)
library("sjlabelled")
library(ggpubr)

#------------------------------------------------------------------------------

# SETTING WORKING DIRECTORY & IMPORTING DATA

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
raw_data <- read_sav("raw_data.sav")
#head(csv_data)
#tail(csv_data)

#------------------------------------------------------------------------------

# RENAMING & SELECTING VARIABLES OF INTEREST

data1 <- raw_data %>% 
  rename(
    int_use1 = UTAUT1_1, #intention to use
    int_use2 = UTAUT1_2, #intention to use
    priv_concerns1 = ReactancePrivacy_6, #privacy concerns
    priv_concerns2 = ReactancePrivacy_7, #privacy concerns
    share_data1 = ReactancePrivacy_8, #willingness to share data
    share_data2 = ReactancePrivacy_9  #willingness to share data
)

new_df = subset(data1, select = c(Random_ID,
                                  Gender,
                                  Age,
                                  Education,
                                  int_use1,
                                  int_use2,
                                  priv_concerns1, 
                                  priv_concerns2,
                                  share_data1,
                                  share_data2,
                                  cond,
                                  Manipchck_virulentie_1,
                                  Manipchck_bewezen_1,
                                  Manipchck_privacy_1,
                                  Manipchck_privacy_2,
                                  Credibility_realism_1)) 

#------------------------------------------------------------------------------

# REMOVING ROWS WITH MISSING VALUES & PARTICIPANTS WISHING TO DELETE THEIR DATA

new_df <- na.omit(new_df)
new_df <- new_df[!(new_df$Random_ID == "17242") & !(new_df$Random_ID == "49333") & !(new_df$Random_ID == "86276"),]

#------------------------------------------------------------------------------

# RELABELLING LIKERT DATA (values from 1-8 into 1-7)

scale_recoding <- function(column) {
  #creating column with fixed values
  column <- car::recode(column, "1=1; 2=2; 3=3; 4=4; 5=5; 7=6; 8=7") 
  #changing labels
  column <- set_labels(column, labels = c("Helemaal mee oneens",
                                         "Oneens",
                                         "Een beetje oneens",
                                         "Neutraal",
                                         "Een beetje mee eens",
                                         "Mee eens",
                                         "Helemaal mee eens"))
  return(column)
}

scale_reversing <- function(column) {
  column <- car::recode(column, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
  column <- set_labels(column, labels = c("Helemaal mee oneens",
                                          "Oneens",
                                          "Een beetje oneens",
                                          "Neutraal",
                                          "Een beetje mee eens",
                                          "Mee eens",
                                          "Helemaal mee eens"))
}

#intention to use variables
new_df$int_use1 <- scale_recoding(new_df$int_use1)
new_df$int_use2 <- scale_recoding(new_df$int_use2)

#privacy concerns variables
new_df$priv_concerns1 <- scale_recoding(new_df$priv_concerns1)
new_df$priv_concerns2 <- scale_recoding(new_df$priv_concerns2)

#data sharing variables
new_df$share_data1 <- scale_recoding(new_df$share_data1)
new_df$share_data2 <- scale_recoding(new_df$share_data2)
new_df$share_data2 <- scale_reversing(new_df$share_data2)

#------------------------------------------------------------------------------

# CREATING COLUMNS PER EXPERIMENTAL VARIABLE

sub_conds <- c("VirHoog","Bewezen","WelKeuze")
new_df["Severity"] <- ifelse(str_detect(new_df$cond,sub_conds[1]), "High", "Low")
new_df["Effectiveness"] <- ifelse(str_detect(new_df$cond,sub_conds[2]), "Strong", "Weak")
new_df["DataSharingOption"] <- ifelse(str_detect(new_df$cond,sub_conds[3]), "Voluntary", "Mandatory")

#------------------------------------------------------------------------------

# DESCRIPTIVE STATISTICS PER COLUMN

means_and_sd <- function(x) {
  c(Mean = mean(x), SD = sd(x))
}
sapply(subset(new_df, select = c(int_use1,
                                 int_use2,
                                 priv_concerns1,
                                 priv_concerns2,
                                 share_data1,
                                 share_data2)), means_and_sd)

#------------------------------------------------------------------------------

# CHECKING INTERNAL RELIABILITY

alpha_score <- function(df_subset,reverse = FALSE) {
  if (reverse == FALSE) {
    return(psych::alpha(df_subset))
  } else {
    return(psych::alpha(df_subset, check.keys = TRUE))
  }
}
#intention to use (1 & 2)
alpha_score(subset(new_df, select = c(int_use1, int_use2)))
#privacy concerns (1 & 2)
alpha_score(subset(new_df, select = c(priv_concerns1, priv_concerns2)))
#data sharing (1 & 2)
alpha_score(subset(new_df, select = c(share_data1, share_data2)))

#------------------------------------------------------------------------------

# CREATING CONSTRUCTS USING UNWEIGHTED AVERAGE

unweighted_av_constructs <- function(a,b) {
  return ((a + b) / 2)
}

#'intention to use' construct
new_df$intention_use <- unweighted_av_constructs(new_df$int_use1, new_df$int_use2)
#'privacy concerns' construct
new_df$privacy_concerns <- unweighted_av_constructs(new_df$priv_concerns1, new_df$priv_concerns2)
#'willingness to share data' construct
new_df$willingness_data <- unweighted_av_constructs(new_df$share_data1, new_df$share_data2)

#descriptive statistics for new constructs
sapply(subset(new_df, select = c(intention_use,
                                 privacy_concerns,
                                 willingness_data)), means_and_sd)

#------------------------------------------------------------------------------

# MEAN CENTERING DATA

mean_centering <- function(x) {
  return(x - mean(x))
}
# mean-centered 'intention to use' (may not be used after all)
new_df$centered_intention <- mean_centering(new_df$intention_use)
# mean-centered 'privacy concerns'
new_df$centered_privacy <- new_df$privacy_concerns-mean(new_df$privacy_concerns)
# mean-centered 'willingness to share data' (may not be used after all)
new_df$centered_willingness <- new_df$willingness_data-mean(new_df$willingness_data)

#------------------------------------------------------------------------------

# MANIPULATION CHECKS

# calculating means and SDs
sapply(subset(new_df, select = c(Manipchck_virulentie_1,
                                 Manipchck_bewezen_1,
                                 Manipchck_privacy_1,
                                 Manipchck_privacy_2)),
                                 means_and_sd)

# function to retrieve conditions of experimental variables and perform t-test
manip_test <- function(df, exp_var, cond1, cond2, manipcheck) {
  col1 = df[df[,exp_var] == cond1,]
  col2 = df[df[,exp_var] == cond2,]
  return(t.test(col1[,manipcheck], 
         col2[,manipcheck], 
         alternative = "two.sided", 
         var.equal = FALSE))
}

# Disease severity
manip_test(new_df, "Severity", "High", "Low", "Manipchck_virulentie_1")
means_and_sd(new_df[new_df$Severity == "High",]$Manipchck_virulentie_1)
means_and_sd(new_df[new_df$Severity == "Low",]$Manipchck_virulentie_1)

# EBET
manip_test(new_df, "Effectiveness", "Strong", "Weak", "Manipchck_bewezen_1")
means_and_sd(new_df[new_df$Effectiveness == "Strong",]$Manipchck_bewezen_1)
means_and_sd(new_df[new_df$Effectiveness == "Weak",]$Manipchck_bewezen_1)

# Data Sharing Options
manip_test(new_df, "DataSharingOption", "Voluntary", "Mandatory", "Manipchck_privacy_1")
manip_test(new_df, "DataSharingOption", "Voluntary", "Mandatory", "Manipchck_privacy_2")
means_and_sd(new_df[new_df$DataSharingOption == "Voluntary",]$Manipchck_privacy_1)
means_and_sd(new_df[new_df$DataSharingOption == "Mandatory",]$Manipchck_privacy_1)
means_and_sd(new_df[new_df$DataSharingOption == "Voluntary",]$Manipchck_privacy_2)
means_and_sd(new_df[new_df$DataSharingOption == "Mandatory",]$Manipchck_privacy_2)

# Credibility check
sapply(subset(new_df, select = c(Credibility_realism_1)), means_and_sd)

#------------------------------------------------------------------------------

# RANDOMIZATION TESTS

# performing chi-square tests of independence
# Disease severity 
chisq.test(new_df$Severity, new_df$Age, correct = FALSE)
chisq.test(new_df$Severity, new_df$Gender, correct = FALSE)
chisq.test(new_df$Severity, new_df$Education, correct = FALSE)

# EBET
chisq.test(new_df$Effectiveness, new_df$Age, correct = FALSE)
chisq.test(new_df$Effectiveness, new_df$Gender, correct = FALSE)
chisq.test(new_df$Effectiveness, new_df$Education, correct = FALSE)

# Data sharing options
chisq.test(new_df$DataSharingOption, new_df$Age, correct = FALSE)
chisq.test(new_df$DataSharingOption, new_df$Gender, correct = FALSE)
chisq.test(new_df$DataSharingOption, new_df$Education, correct = FALSE)

#------------------------------------------------------------------------------

# RQ1 - PRIVACY CONCERNS ON INTENTION TO USE (WITH EFFECTIVENESS AND VIRULENCE AS MODERATORS)

# (CODE BASED ON https://statsnotebook.io/blog/analysis/moderation_interaction_regression/)
model1 <- lm(intention_use ~ centered_privacy 
             + Effectiveness
             + Severity
             + centered_privacy*Effectiveness 
             + centered_privacy*Severity, data = new_df)
summary(model1) 
cbind(coef(model1), confint(model1, level = 0.95))

#displaying standardized residuals & retrieving min-max values
model1.std <- rstandard(model1)
col_m1_std <- cbind(model1.std)
max(col_m1_std)
min(col_m1_std)
png(filename = "M1_std_res.png", width = 6, height = 4, units = "in", res = 400)
par(family = "serif")
plot(model1.std, ylab = "Standardized Residuals")
dev.off()

#displaying diagnostic plots
png(filename = "M1_diagnostic_plots.png", width = 9, height = 4, units = "in", res = 400)
par(mfrow = c(2,3), family = "serif", cex.main = 0.5)
plot(model1, which = 1:6)
dev.off()

#outlier Test
outlierTest(model1)

#calculating VIF (for multicollinearity) and error term
vif(model1) 
mean(resid(model1)) 

#plotting results
png(filename = "M1_viru.png", width = 6, height = 4, units = "in", res = 400)
ggplot(new_df, aes(centered_privacy, intention_use, color = factor(Severity))) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(name = "Severity of \ndisease", values = c("#B59dfa", "#ce94bc")) +
  labs(y= "Intention to use", x = "Privacy concerns") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 20, family = "serif"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))
dev.off()

png(filename = "M1_effect.png", width = 6, height = 4, units = "in", res = 400)
ggplot(new_df, aes(centered_privacy, intention_use, color = factor(Effectiveness))) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(name = "Evidence for \neffectiveness", values = c("#007acc", "#ff99ff")) +
  labs(y= "Intention to use", x = "Privacy concerns") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 20, family = "serif"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))
dev.off()

#simple slope analysis using estimated marginal means (EMMs)
m_centered_privacy <- mean(new_df$centered_privacy, na.rm = TRUE)
sd_centered_privacy <- sd(new_df$centered_privacy, na.rm = TRUE)

#simple slope for Disease Severity moderator (False = Low Severity)
emm <- emmeans(model1, ~ centered_privacy*Severity, cov.keep = 2, 
                   at = list(centered_privacy = c(m_centered_privacy - sd_centered_privacy, 
                                                  m_centered_privacy + sd_centered_privacy)),
                   level = 0.95)
summary(emm)
simpleSlopes <- emtrends(model1, pairwise ~ Severity, var = "centered_privacy", 
                         level = 0.95) |> test() # to show CIs, remove |> test()
summary(simpleSlopes)

#------------------------------------------------------------------------------

# RQ2 - DATA SHARING ON PRIVACY CONCERNS

#running an independent 2-sample t-test
m2_t_Test <- t.test(new_df$privacy_concerns ~ new_df$DataSharingOption, alternative = "two.sided", var.equal = FALSE)
m2_t_Test

#assumption checks
res.ftest <- var.test(privacy_concerns ~ DataSharingOption, data = new_df)
res.ftest

new_df %>% 
  group_by(DataSharingOption) %>%
  get_summary_stats(privacy_concerns, type = "mean_sd")


#plotting model
png(filename = "M2_boxplot.png", width = 6, height = 4, units = "in", res = 400)
par(family = "serif")
boxplot(privacy_concerns ~ DataSharingOption,
        #names = c("Mandatory", "Voluntary"),
        data=new_df,
        main=" ",
        xlab="Data Sharing Option",
        ylab="Privacy Concerns",
        col = "#c8d3b8",
        border="black",
        par(cex.lab = 1.5, cex.axis = 1, cex.main = 1.2)
)
dev.off()

#------------------------------------------------------------------------------

# RQ3 - EBET AND DISEASE SEVERITY ON WILLINGNESS T0 SHARE DATA

#box plot to plot the data grouped by the combinations of the levels of the two factors.
png(filename = "anova.png", width = 6, height = 4, units = "in", res = 400)
ggboxplot(new_df, x = "Severity",
          y = "willingness_data",
          color = "Effectiveness",
          palette = c("#B59dfa", "#c8d3b8")) + 
  labs(y= "Willingness to share data") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 20, family = "serif"))
dev.off()

res.aov1 <- aov(willingness_data ~ Severity + Effectiveness, data = new_df)
summary(res.aov1)

#descriptive statistics
model.tables(res.aov1, type="means", se = TRUE)

#assumption checks
png(filename = "M3_assumptions.png", width = 6, height = 4, units = "in", res = 400)
par(mfrow = c(1,2), family = "serif", cex.main = 0.5)
plot(res.aov1, 1)
plot(res.aov1, 2)
dev.off()

