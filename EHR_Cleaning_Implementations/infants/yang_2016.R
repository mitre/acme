################################################################################
# yang_hutcheon_2015.R: Implementation of paper to compare data cleaning 
# algorithms for infant data. Original paper is by Seungmi Yang and Jennifer
# Hutcheon and uses conditional growth percentiles "to systematically identify 
# implausible measurements in growth trajectory data". The conditional growth 
# percentiles are computed using a random effects model. The algorithm in the
# original paper is used to clean data for children from birth to 6.5 years old.
# The code below is an R implementation of the author's original Stata code 
# (contained in Appendix A) of the paper. The original paper is available at:
# https://www.sciencedirect.com/science/article/pii/S1047279715004184?viewFullText=true
# 
# @author = Max Olivier
################################################################################

# Import necessary libraries
library(dplyr)
library(dbplyr) 
library(ggplot2)
library(hrbrthemes)
library(miceadds)
library(broom)
library(lme4)
library(data.table)

################################################################################
# Import and clean data----
# At end data should be in three colummns: ID for 
################################################################################

# Import synthetic data set for testing.

dat <- fread("C:/Users/molivier/Documents/Projects/CDC_CODI/Data_sets/gc-observations.csv")

# Filter out the data in several ways. Just keep weight data (since original
# algorithm uses weight data) and drop all the columns
df <- dat[param=="WEIGHTKG", c("subjid", "measurement", "sex",  "age_days")]




################################################################################
# Drop missing observations and sort data.----
################################################################################

df <- df[!(is.na(df$measurement) | is.na(df$age_days)),]
df <- df[order(df$subjid, df$age_days),]

# Doesn't work
df$visit <- data.table::rowid(df$dubjid)

# Works
df[, visit := seq_len(.N), by=subjid]
df[, visit := rowid(subjid)]

# Other alternatives to generate the visit variable.
# df$num <- ave(df$val, df$cat, FUN = seq_along)
# df %>% group_by(cat) %>% mutate(id = row_number())
# df$num <- sequence(rle(df$cat)$lengths) with as.character() around cat if it is a factor var
# df %>% arrange(cat, val) %>% group_by(cat) %>% mutate(id = row_number())
# df %>% group_by(cat) %>% mutate(num = 1:n())
# df$num <- data.table::rowid(df$cat)
# For entire data frame: iris %>% mutate(row_num = seq_along(Sepal.Length))
# For groups: iris %>% group_by(Species) %>% mutate(num_in_group=seq_along(Species)) %>% as.data.frame


# Include cubic spline, though not sure what this is for

# Random effects model for unconditional mean weight by age (equation 1 from 
# Appendix A)
model = lmer(measurement ~ (1 + age_days | subjid) + age_days, data=df, REML = FALSE)


# Get predicited value
df$uncond_mean <- predict(model)

summary(model)

str(model)

summary(model12)

X <- VarCorr(model)


str(summary(model)$varcor)





# Run Hierarchical Linear Models ----

library(lme4)
library(lmerTest)

pl_local <- as.data.frame(personlevel)
pl_local$eri <- factor(pl_local$eri,
                       levels=c("White", "AAPI", "Black", "Hispanic", "Native",
                                "Other"))
pl_local$year <- factor(pl_local$year)

pl_local %>% select(year) %>% group_by(year) %>% count()

head(pl_local)

## Dependent variable: PERFORMANCE ----

model1=lmer(score ~ (1 | year), data=pl_local, REML = FALSE)
summary(model1)

model2=lmer(score ~ (1 + unionize | year) + pay_plan_desc + sex_cd + age 
            + years_federal + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos 
            + sat_wl_pos + eri + veteran_status + supervisor_status + unionize,
            data=pl_local,
            REML = TRUE)
summary(model2)


model2 <- lm(score ~ year, data=pl_local)
summary(model2)




model2 <- lm(score ~ year+pay_plan_desc + sex_cd + age 
             + years_federal + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos 
             + sat_wl_pos + eri + veteran_status + supervisor_status + unionize, data=pl_local)
summary(model2)



### Random intercept only model 
model11=lmer(score ~ (1 | center) + pay_plan_desc + sex_cd + age 
             + years_federal + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos 
             + sat_wl_pos + eri + veteran_status + supervisor_status + unionize,
             data=pl_local,
             REML = FALSE)

summary(model11)

### Mixed effects model
model12=lmer(score ~ (1 + sex_cd + unionize | center) + pay_plan_desc + sex_cd + age 
             + years_federal + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos 
             + sat_wl_pos + eri + veteran_status + supervisor_status + unionize,
             data=pl_local,
             REML = TRUE)

summary(model12)


## Dependent variable: RETIREMENT ----

# personlevel_retire <- pl_local[pl_local['retirement_elig'] == 1, ]
# 
# model2 <- glmer(formula = retirement ~ (1 | center) + pay_plan_desc + sex_cd + age + job_fam
#                 + years_federal + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos 
#                 + sat_wl_pos + eri + veteran_status + supervisor_status + unionize,
#                 family = binomial(link="logit"),
#                 data = personlevel_retire,
#                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
# 
# summary(model2)


## Dependent variable: RESIGNATION ----

# personlevel_resign <- pl_local[pl_local['retirement_elig'] == 0, ]
# 
# model3 <- glmer(formula = resignation ~ (1 | center) + pay_plan_desc + sex_cd + age + job_fam
#                 + years_federal + retirement_elig + promotion + rec_work_pos + sat_job_pos + sat_pay_pos + sat_tel_pos
#                 + sat_wl_pos + eri + veteran_status + supervisor_status + unionize,
#                 family = binomial(link="logit"),
#                 data = pl_local,
#                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
# 
# summary(model3)


# Disconnect from database ----
dbDisconnect(con)
