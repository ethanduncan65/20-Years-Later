#install package for cv.glm()
library(boot)

# load the data
df = read.csv('MLB Team Stats 2015-2019.csv', header = TRUE)
# reduce data to variables of interest
df_playoff = df[-c(3:6, 8:9, 13, 15:20, 27)]

# quick EDA
pairs(df_playoff) ### {win_pct vs. run_diff}
# plot made_playoffs against likely predictors
par(mfrow=c(2,2))
plot(df_playoff$win_pct, df_playoff$made_playoffs)
plot(df_playoff$vsWO_win_pct, df_playoff$made_playoffs)
plot(df_playoff$road_win_pct, df_playoff$made_playoffs)
plot(df_playoff$vsLHP_win_pct, df_playoff$made_playoffs)



# SLR model for win_pct = payroll_M
slr_model_payroll = lm(df_playoff$win_pct ~ df_playoff$payroll_M)
slr1_summ = summary(slr_model_payroll)
# plot data against regression eq.
plot(df_playoff$payroll_M, df_playoff$win_pct)
abline(a=slr1_summ$coefficients[1], b=slr1_summ$coefficients[2], col='red')
# make prediction for Mariner's win_pct
pred_1 = slr1_summ$coefficients[1] + slr1_summ$coefficients[2]*(81.257217)
pred_1

# SLR model for win_pct = Rdiff
slr_model_runDiff = lm(df_playoff$win_pct ~ df_playoff$Rdiff)
summary(slr_model_runDiff)

# LOOCV on orders of payroll_M
x = df_playoff$payroll_M
y = df_playoff$win_pct
# df consisting of payroll_M and win_pct only
df = data.frame(x,y)
# vector to store error for each model
errors = c()
# for loop over the 7 models we want to fit
for (i in 1:7){
  set.seed(99)
  # fit model based on degree of polynomial
  fit.glm.i <- glm(y~poly(x,i))
  # extract and append error to storage vector
  i_err = cv.glm(df, fit.glm.i)$delta[1]
  errors = c(errors, i_err)
}
# plot the errors
plot(c(1:7),errors, xlab = "degree of polynomial", ylab = "error")
lines(c(1:7),errors)



# logistic regression model to predict odds of making playoffs using only payroll_M
logit_modelX = glm(df_playoff$made_playoffs ~df_playoff$payroll_M, 
                   family = binomial(link = "logit"))
summary(logit_modelX)
cv.glm(df_playoff, logit_modelX)$delta[1]

# logistic regression model to predict odds of making playoffs
logit_model = glm(df_playoff$made_playoffs ~ df_playoff$road_win_pct 
                  + df_playoff$vsWO_win_pct 
                  + df_playoff$payroll_M, family = binomial(link = "logit"))
logit_1 = summary(logit_model)
cv.glm(df_playoff, logit_model)$delta[1]

# calc. current season stats for Mariners needed for prediction
SEA2021_road_win_pct = 14/(14+20)
SEA2021_vsWO_win_pct = 15/(15+20)
# prediction for Seattle Mariners
lnOdds = logit_1$coefficients[1] + logit_1$coefficients[2]*(SEA2021_road_win_pct) 
      + logit_1$coefficients[3]*(SEA2021_vsWO_win_pct) 
      + logit_1$coefficients[4]*(81.257217)
pred_prob = exp(lnOdds)/(1+exp(lnOdds))

# calc. current season stats for Boston Red Sox needed for prediction
BOS2021_road_win_pct = 20/(20+10)
BOS2021_vsWO_win_pct = 17/(17+11)
# prediction for Boston Red Sox
lnOdds_BOS = logit_1$coefficients[1] + logit_1$coefficients[2]*(BOS2021_road_win_pct) 
      + logit_1$coefficients[3]*(BOS2021_vsWO_win_pct) 
      + logit_1$coefficients[4]*(176.846501)
pred_prob_BOS = exp(lnOdds_BOS)/(1+exp(lnOdds_BOS))


