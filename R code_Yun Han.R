
library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(ggthemr)
library("geepack")
library(nlme)
library("ggpubr")
library(aod)
library(MASS)
library(pscl)

## Import ggplot theme
ggthemr("fresh", type = "outer")
### Read dta data in R
mydata <- read_dta(file = "200cTermProject.dta") 

### Data preprocessing: 
# 1.1 convert time format to Data
mydata$visitdat <- as.Date(mydata$visitdat,format='%m/%d/%Y')
# 1.2 remove data if visitdata or haq is NA 
mydata <- filter(mydata, is.na(mydata$visitdat) == FALSE & is.na(mydata$haq) == FALSE)

# 1.3 Find the baseline of each patient and count the number of visit
# use the difference between baseline and visitdate to category the number of visit:count
# 1.4 There are 32 rows, 16 pairs share the same value, we save the first one in each pair and delete the others 

dpen <- mydata %>% 
  group_by(patid) %>% 
  arrange(patid, visitdat) %>%
  mutate(count = round((visitdat-min(visitdat))/183)+1) %>%
  distinct(count, .keep_all = TRUE) 

# 1.5 There are also 9 records out of the 5 max range, we deleted these records.
dpen <- filter(dpen, count <= 5)

# 1.6 Convert the format from long to wide.
wide <- dpen %>%
  dplyr::select(patid, count, haq) %>%
  spread(key = count, value = haq)

# mutate the variable group on the wide dataset
wide <- dpen1 %>% select(patid, group) %>%
  inner_join(wide)

# Convert the format from long to wide in complete data.
wide_complete <- complete %>%
  select(patid, count, haq) %>%
  spread(key = count, value = haq)

wide_complete <- complete1 %>% select(patid, group) %>%
  inner_join(wide_complete)

#### Q1: summary of data
## 1.1 percentage of patients who completed all the visit
# Calculate the percentage of patients who completed all the schedule visit
num <- dpen %>% group_by(patid) %>%
  summarize(number = n())

percent1 <- num %>% group_by(number) %>% 
  summarize(count = n()) %>%
  mutate(percent = count/134)

## 1.2 percentage of patients who showed up at each visit
percent2 <- dpen %>% group_by(count) %>%
  summarize(number = n()) %>%
  mutate(percent = number/134)
# plot the percent-count plot : we can see that percentages of patients who showed up at each visit is decreasing
ggplot(percent2, aes(x = count, y = percent)) +
  geom_line() +
  ylim(0, 1) +
  xlab("Count") + ylab("Percentage") + ggtitle("Percentages of Patients Who Showed Up at Each Visit")

## 1.3  distribution of demographic variables for the whole cohort
# Seperate the data into complete data & non-complete
five <- filter(num, number == 5)
complete <- inner_join(dpen, five)
noncomplete <- dpen[!dpen$patid %in% complete$patid,]

# Calculate each person's demographic variable
complete1 <- complete %>%
  group_by(patid) %>%
  summarize(age=round(mean(age)), weight=mean(weight),
            race=mean(race), group=mean(group),
            sex=mean(sex))

dpen1 <- dpen %>%
  group_by(patid) %>%
  summarize(age=round(mean(age)), weight=mean(weight),
            race=mean(race), group=mean(group),
            sex=mean(sex), sbp = mean(sbp),
            cpk = mean(cpk), cardrate = mean(cardrate)) %>%
  mutate(complete = case_when(patid %in% complete1$patid ~ 1, TRUE ~ 0))

noncomplete1 <- noncomplete %>%
  group_by(patid) %>%
  summarize(age=round(mean(age)), weight=mean(weight),
            race=mean(race), group=mean(group),
            sex=mean(sex))
# distribution of whole cohort:
# race
table(dpen1$race) 
table(complete1$race) 
table(noncomplete1$race) 
hist(complete1$race)

ggplot(dpen1, aes(x=race))+
  geom_histogram(color="black", fill="white", binwidth=25)+
  facet_grid(complete ~ race)


# group 
table(dpen1$group)
table(complete1$group)
table(noncomplete1$group)
prop.table(table(dpen1$group))


# age
summary(dpen1$age)
summary(complete1$age)
summary(noncomplete1$age)
lmts <- range(complete1$age, noncomplete1$age)
par(mfrow = c(1, 2))
boxplot(complete1$age, xlab = "completer", ylab = "Age", ylim=lmts)
boxplot(noncomplete1$age,xlab = "non-completer",  ylim=lmts)

# sex
table(dpen1$sex) 
table(complete1$sex) 
table(noncomplete1$sex) 
prop.table(table(dpen1$sex))
ggplot(dpen1, aes(x=sex))+
  geom_histogram(color="black", fill="white", binwidth=20)+
  facet_grid(complete ~ sex)


# weight
summary(complete$weight)
lmts <- range(complete$weight, noncomplete$weight)
par(mfrow = c(1, 2))
boxplot(complete$weight, xlab = "completer", ylab = "Weight", ylim=lmts)
boxplot(noncomplete$weight,xlab = "non-completer",  ylim=lmts)

# sbp
summary(complete$sbp)
lmts <- range(complete$sbp, noncomplete$sbp)
par(mfrow = c(1, 2))
boxplot(complete$sbp, xlab = "completer", ylab = "Systolic blood pressure", ylim=lmts)
boxplot(noncomplete$sbp,xlab = "non-completer",  ylim=lmts)
# haq
summary(complete$haq)
lmts <- range(complete$haq, noncomplete$haq)
par(mfrow = c(1, 2))
boxplot(complete$haq, xlab = "completer", ylab = "HAQ", ylim=lmts)
boxplot(noncomplete$haq,xlab = "non-completer",  ylim=lmts)

## 1.4 display patients’ responses over time 
haq_mean <- dpen %>% 
  group_by(count) %>% 
  summarize(HAQ_mean = mean(haq))

ggplot(haq_mean, aes(x = count, y = HAQ_mean)) +
  geom_point() + geom_line() + ggtitle("Patients\' average HAQ over time")

# identify differences in patients treated in the two groups.
haq_mean1 <- dpen %>% 
  group_by(group, count) %>% 
  summarize(HAQ_mean = mean(haq))

ggplot(data=haq_mean1,
       aes(x=count, y=HAQ_mean, color=group, group = group)) +
  geom_point() + geom_line() + ggtitle("Patients\' average HAQ over time in different group") 


#### Q2:
# mutate the variable group on the wide dataset
wide <- dpen1 %>% select(patid, group) %>%
  inner_join(wide)


library("geepack")
### Method 1: Using GLM model: use all the haq data
## scatter plot matrix
my_cols <- c( "#00AFBB", "#FC4E07")  
pairs(wide[,3:7], pch = 19,  cex = 0.5,
      col = c("red", "green3")[wide$group])

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.1)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, cex = 0.7, col = my_cols[wide_complete$group])
}
# Create the plots
pairs(wide_complete[,3:7], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# There is one patid had word(2004A), need to convert the patid into numeric. 
dpen_n <- transform(dpen,subject=as.numeric(factor(patid)))
# 1. Independence Working Covariance
gee.ind<-geeglm(haq ~ group + count,family=gaussian,
                data = dpen_n,id = subject,wave = count,corst = "independence")
summary(gee.ind)
anova(gee.ind)

# 2. Exchangeable Working Covariance
gee.exch<-geeglm(haq ~ group + count,family=gaussian,
                data = dpen_n,id = subject,wave = count,corst = "ex")
summary(gee.exch)
anova(gee.exch)

# 3. AR(1) Working Covariance
gee.ar1<-geeglm(haq ~ group + count,family=gaussian,
                 data = dpen_n,id = subject,wave = count,corst = "ar1")
summary(gee.ar1)
anova(gee.ar1)

# 4. unstructured
#gee.un<-geeglm(haq ~ group + count,family=gaussian,
#                data = dpen,id = patid,wave = count,corst = "unstructured")
#summary(gee.un)
#anova(gee.un)

# AIC to compare correlation structures
library(nlme)
ind<-corIdent(form = ~ 1 | patid)
gls.ind<-gls(haq ~ group + count, data=dpen_n, correlation=ind)

exch<-corCompSymm(form = ~ 1 | patid) 
gls.exch<-gls(haq ~ group + count, data=dpen_n, correlation=exch)

ar1<-corAR1(form = ~ 1 | patid) 
gls.ar1<-gls(haq ~ group + count, data=dpen_n, correlation=ar1)

un<-corSymm(form = ~ 1 | patid) 
gls.un<-gls(haq ~ group + count, data=dpen_n, correlation=un)

AIC(gls.ind,gls.exch,gls.ar1,gls.un)

### Method 2: Using pair t- test: use only baseline and final haq data

## 2.1 use only available data (i.e. patients who had both baseline and 2-year data)
# step 1: summary the data
m1 <- wide %>% select(patid, group, "1", "5") %>%
  filter(is.na(wide$"5") == FALSE) %>%
  mutate(diff = `1` - `5`)

m1_sum <- group_by(m1, group) %>%
  summarise(
    count = n(),
    mean = mean(diff, na.rm = TRUE),
    sd = sd(diff, na.rm = TRUE)
  )

# step 2:Visualize your data using box plots
library("ggpubr")
ggboxplot(m1, x = "group", y = "diff", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "diff", xlab = "Groups")

# step 3: normal assumption check
# Shapiro-Wilk normality test for group 0's HAQ diff
qwe <- with(m1, shapiro.test(diff[group == "0"]))# p = 0.3
# Shapiro-Wilk normality test for group 1's HAQ diff
qwe <- with(m1, shapiro.test(diff[group == "1"])) # p = 0.04

# step 4: Do the two populations have the same variances?
res.ftest <- var.test(diff ~ group, data = m1) # p = 0.703
res.ftest

# step 5: Compute unpaired two-samples t-test
res <- t.test(diff ~ group, data = m1, var.equal = TRUE)
res # p = 0.594


## 2.2 use data from completers only (i.e. patients who showed up for all scheduled visits)
# step 1: summary the data
m2 <- wide_complete %>% select(patid, group, "1", "5") %>%
  mutate(diff = `1` - `5`)

m2_sum <- group_by(m2, group) %>%
  summarise(
    count = n(),
    mean = mean(diff, na.rm = TRUE),
    sd = sd(diff, na.rm = TRUE)
  )

# step 2:Visualize your data using box plots
library("ggpubr")
ggboxplot(m2, x = "group", y = "diff", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "diff", xlab = "Groups")

# step 3: normal assumption check
# Shapiro-Wilk normality test for group 0's HAQ diff
qwe <- with(m2, shapiro.test(diff[group == "0"]))# p = 0.3
# Shapiro-Wilk normality test for group 1's HAQ diff
qwe <- with(m2, shapiro.test(diff[group == "1"])) # p = 0.05
qwe
# step 4: Do the two populations have the same variances?
res.ftest2 <- var.test(diff ~ group, data = m2) # p = 0.736
res.ftest2

# step 5: Compute unpaired two-samples t-test
res2 <- t.test(diff ~ group, data = m2, var.equal = TRUE)
res2 # p = 0.493


## 2.3 if patients have missing data at 2 years after baseline, impute them by carrying forward the last available data to 2 years
# step 1: summary the data
# the count of last record row
new_wide <- wide[!rowSums(is.na(wide))==4,]
newwide <- data.matrix(new_wide)
ind <- !is.na(newwide) 
last <- tapply(newwide[ind], row(newwide)[ind], tail, 1) 

m3 <- new_wide %>%
  mutate(diff = `1` - last)

m3_sum <- group_by(m3, group) %>%
  summarise(
    count = n(),
    mean = mean(diff, na.rm = TRUE),
    sd = sd(diff, na.rm = TRUE)
  )

# step 2:Visualize your data using box plots
library("ggpubr")
ggboxplot(m3, x = "group", y = "diff", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "diff", xlab = "Groups")

# step 3: normal assumption check
# Shapiro-Wilk normality test for group 0's HAQ diff
qwe <- with(m3, shapiro.test(diff[group == "0"]))# p = 0.56
# Shapiro-Wilk normality test for group 1's HAQ diff
qwe <- with(m3, shapiro.test(diff[group == "1"])) 

# step 4: Do the two populations have the same variances?
res.ftest3 <- var.test(diff ~ group, data = m3) # p = 0.086
res.ftest3

# step 5: Compute unpaired two-samples t-test
res3 <- t.test(diff ~ group, data = m3, var.equal = TRUE)
res3 # p = 0.493


## 2.4 work with data only collected at visit dates that are within 10 days of the scheduled visit date; 
m4 <- dpen %>% 
  group_by(patid) %>% 
  arrange(patid, visitdat) %>%
  mutate(time = (visitdat-min(visitdat))) %>%
  filter(time >= 720 & time <= 740) %>% 
  select(patid) %>% 
  inner_join(wide) %>%
  mutate(diff = `1` - `5`)

m4_sum <- group_by(m4, group) %>%
  summarise(
    count = n(),
    mean = mean(diff, na.rm = TRUE),
    sd = sd(diff, na.rm = TRUE)
  )

# step 2:Visualize your data using box plots
library("ggpubr")
ggboxplot(m4, x = "group", y = "diff", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "diff", xlab = "Groups")

# step 3: normal assumption check : failed
# Shapiro-Wilk normality test for group 0's HAQ diff
qwe <- with(m4, shapiro.test(diff[group == "0"]))# p = 0.4
# Shapiro-Wilk normality test for group 1's HAQ diff
qwe <- with(m4, shapiro.test(diff[group == "1"])) 

# Step 4: normal assumption failed
# we need to use the non parametric two-samples Wilcoxon rank test.
res4 <- wilcox.test(diff ~ group, data = m4,
                   exact = FALSE)
res4 # p = 0.725

#### Q3: use logistic regression
## Method 1: HAQ scores had dropped by at least 30% at the end of the 24th month
# We use the m1 data, which contained patient finished the first and fifth data
library(aod)
binary <- m2 %>% mutate(drug_effi = diff/`1`) %>%
  mutate(response = case_when(drug_effi >= 0.3 ~ 1, TRUE ~ 0)) %>%
  left_join(dpen1)

# 1. logistic regression with only one variable:group
logit1 <- glm(response ~ group, data = binary, family = "binomial")
summary(logit1)

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 2)
# Result: The chi-squared test statistic of 0.062, with one degrees of freedom is associated with a p-value of 0.8 indicating that the overall effect of group is not statistically significant.

# 2. logistic regression with multiple variable:group cardratio, sbp and cpk
# find mean of each covariate of patient. 
dat <- dpen %>%
  filter(is.na(sbp)==FALSE & is.na(cpk)==FALSE & is.na(cardrate)==FALSE) %>%
  group_by(patid) %>%
  summarize(msbp = mean(sbp), mcpk = mean(cpk), mcardrate = mean(cardrate))

newbinary <- binary %>%
  left_join(dat, by = "patid")

#logistic regression with multiple variable:group, cardratio, sbp and cpk
logit2 <- glm(response ~ group + msbp + mcpk + mcardrate, 
              data = newbinary, family = "binomial")
summary(logit2)


wald.test(b = coef(logit2), Sigma = vcov(logit2), Terms = 2)

## Method 2: break up the HAQ variable into 3 categories: low, moderate and high
multi <- m2 %>% 
  mutate(response = case_when(diff >= 1 ~ "high",
                              diff >= 0.5 & diff < 1 ~ "moderate",
                              diff < 0.5 ~ "low")) %>%
  left_join(dpen1)

# 1. ordered logistic regression with only one variable:group
library(MASS)

multi$response <- factor(multi$response, levels=c("low","moderate","high"))
logit3 <- polr(response~ group,
               data=multi, Hess=TRUE)
summary(logit3)

# calculate the p-value

summary_table <- coef(summary(logit3))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


# 2. depend on more covariates.
multi_new <- multi %>%
  left_join(dat, by = "patid")

#propotional odds model(ordinal) with multiple variable:group, cardratio, sbp and cpk
multi_new$response <- factor(multi_new$response, levels=c("low","moderate","high"))
logit4 <- polr(response~ group + msbp + mcpk + mcardrate,
               data=multi_new, Hess=TRUE)
summary(logit4)

summary_table <- coef(summary(logit4))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


#### Q4: Find your best fitting model relating nooffalls and baseline variables

library(pscl)
Q4 <- dpen %>%
  dplyr::select(patid, sex, age, race, weight, sbp, group, cardmega, haq, nooffalls, count ) %>%
  na.omit() %>%
  distinct(patid, .keep_all = TRUE) %>%
  filter (count == 1)

Q4$race <- factor(Q4$race, levels=c("0","1","2","3","4"))

dispersion <- var(Q4$nooffalls)/mean(Q4$nooffalls)
# Dispersion is 1.3. It is close to 1, hence use possion model.
# But we can compare the three model.
summary(fullpomod <- glm( nooffalls~ haq + sbp + cardmega + weight
                          + sex + age + race, family="poisson", data=Q4))
AIC(fullpomod)

summary(fullngmod <- glm.nb( nooffalls~ haq + sbp + cardmega + weight
                          + sex + age + race, data=Q4))
AIC(fullngmod)


summary(fullinf <- zeroinfl( nooffalls~ haq + sbp + cardmega + weight
                             + sex + age + race|haq + sbp +cardmega, data=Q4))
AIC(fullpomod, fullngmod, fullinf)
