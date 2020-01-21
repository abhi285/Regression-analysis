
# Read the data



alumni <- read.csv("C:/Users/Vishnu/Desktop/FALL 2019/2ND FLEX/Linear Regression/alumni.csv")

dim(alumni)

# Create temp df to manipulate data

alumni_data <- alumni[,2:11]

alumni_data$private <- as.factor(alumni_data$private)
##alumni_data$State<-as.factor(alumni$State)

# Plot quantitative variables
library(ggplot2)
GGally::ggpairs(alumni_data, mapping = aes(alpha = 0.5)) + theme_light()



flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
cor(alumni_data[,-4])
library(Hmisc)
res2<-rcorr(as.matrix(alumni_data[,-4]))
flattenCorrMatrix(res2$r, res2$P)

# Fit MLR

model_fit <- lm(alumni_giving_rate ~ ., data = alumni_data)
summary(model_fit)
vif(model_fit)

#removing multicollinaerity issues


model_fit_mcol<-lm(alumni_giving_rate ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation , data = alumni_data)
summary(model_fit_mcol)
vif(model_fit_mcol)


sqrt_model_2<-lm(sqrt(alumni_giving_rate) ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation , data = alumni_data)
summary(sqrt_model_2)

log_trans_model<-lm(log(alumni_giving_rate) ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation , data = alumni_data)
summary(log_trans_model)
vif(log_trans_model)


bc <- MASS::boxcox(alumni_giving_rate ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation,  data = alumni_data)



(lambda <- bc$x[which.max(bc$y)])
alumni_data$AGR2 <- (alumni_data$alumni_giving_rate ^ lambda - 1) / lambda
model_fit_boxcox <- lm(AGR2 ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation,  data = alumni_data)

summary(model_fit_boxcox)
vif(model_fit_boxcox)
plot(model_fit_boxcox)

# Plot residuals
plot(fitted(model_fit_boxcox), rstudent(model_fit_boxcox), pch = 19, las = 1,
     
     col = adjustcolor("darkblue", alpha.f = 0.5),
     
     xlab = "Fitted value", ylab = "Studentized residual",
     
     main = "Transformed data")

abline(h = 0, lty = 2,
       
       col = adjustcolor("darkred", alpha.f = 0.5))

install.packages('leaps')
library(leaps)
# All subsets regression (main effects only)
a1 <- regsubsets(AGR2 ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation, data = alumni_data, 
                 nbest = 10, nvmax = 6)
plot(a1, scale = "bic")
library(ggplot2)

# Gather results
res1 <- data.frame(
  "nvar" = apply(summary(a1)$which, 1, FUN = function(x) sum(x) - 1),
  "bic" = summary(a1)$bic,
  "adjr2" = summary(a1)$adjr2
)

p1 <- ggplot(res1, aes(x = nvar, y = bic)) +
  geom_point(alpha = 0.5, size = 2, color = "darkred") +
  stat_summary(fun.y = min, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "BIC")
p2 <- ggplot(res1, aes(x = nvar, y = adjr2)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  stat_summary(fun.y = max, geom = "line", alpha = 0.5, linetype = "dashed") +
  theme_light() +
  labs(x = "Number of predictors", y = "Adjusted R-squared")
gridExtra::grid.arrange(p1, p2, nrow = 2)


#with 3 predictors
summary(best1 <- lm(AGR2 ~ enrollment+National.University.Rankings.2019+Employed.2.years.after.graduation, data = alumni_data))


#evaluating my models

PRESS <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      sum(rstandard(x, type = "predictive") ^ 2)
    })
    names(res) <- as.character(match.call()[-1L])
    res
  } else {
    sum(rstandard(object, type = "predictive") ^ 2)
  }
}

modelMetrics <- function(object, ...) {
  if(!missing(...)) {
    res <- sapply(list(object, ...), FUN = function(x) {
      c("AIC" = AIC(x), "BIC" = BIC(x), 
        "adjR2" = summary(x)$adj.r.squared,
        "RMSE"  = sigma(x), "PRESS" = PRESS(x), 
        "nterms" = length(coef(x)))
    })
    colnames(res) <- as.character(match.call()[-1L])
    res
  } else {
    c("AIC" = AIC(object), "BIC" = BIC(object), 
      "adjR2" = summary(object)$adj.r.squared, 
      "RMSE"  = sigma(object), "PRESS" = PRESS(object),
      "nterms" = length(coef(object)))
  }
}
###Backward selection
#main only;

fit_max_1 <- lm(AGR2 ~ private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation, data = alumni_data)
be_1 <- step(fit_max_1, direction = "backward", 
             trace = 0, k = log(nrow(alumni_data)))
summary(fit_max_1)
#main and two way
fit_max_2 <- lm(AGR2 ~ (private+percent_of_classes_under_20 +student_faculty_ratio +Graduation.Rate+enrollment+Median.Earnings.6.years.after.graduation+National.University.Rankings.2019+Employed.2.years.after.graduation)^2, data = alumni_data)
be_2 <- step(fit_max_2, direction = "backward", 
             trace = 0, k = log(nrow(alumni_data)))

#Forward selection
#main only
fit_min <- lm(AGR2 ~ 1, data = alumni_data)
fs_1 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,
                          upper = fit_max_1),
             trace = 0, k = log(nrow(alumni_data)))

#two way
fs_2 <- step(fit_min, direction = "forward", 
             scope = list(lower = fit_min,
                          upper = fit_max_2),
             trace = 0, k = log(nrow(alumni_data)))

##Stepwise

# Main effects only (i.e., no interactions)
ss_1 <- step(be_1, direction = "both", 
             scope = list(lower = fit_min,
                          upper = fit_max_1),
             trace = 0, k = log(nrow(alumni_data)))
# Main effects and two-way interactions
ss_2 <- step(be_2, direction = "both", 
             scope = list(lower = fit_min,
                          upper = fit_max_2),
             trace = 0, k = log(nrow(alumni_data)))

res <- modelMetrics(be_1, be_2, fs_1, fs_2, ss_1, ss_2)
round(res, digits = 3)

summary(fs_2)
