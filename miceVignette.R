library(mice)
library(lattice)
set.seed(123)

# Inspect incomplete data
nhanes
summary(nhanes)
md.pattern(nhanes)

# Form regression model where age is predicted from bmi
fit <- with(nhanes, lm(age ~ bmi))
summary(fit)

# Impute missing data in nhanes dataset with mean imputation
imp <- mice(nhanes, method = "mean", m = 1, maxit = 1)

# Explore completed dataset
complete(imp)

colMeans(nhanes, na.rm = T)

fit <- with(imp, lm(age ~ bmi))
summary(fit)

densityplot(nhanes$bmi)

# Impute missing data with regression imputation
imp <- mice(nhanes, method = "norm.predict", m = 1, maxit = 1)
complete(imp)
fit <- with(imp, lm(age ~ bmi))
summary(fit)

# Impute with stochastic regession imputation
imp <- mice(nhanes, method = "norm.nob", m = 1, maxit = 1)
complete(imp)
fit <- with(imp, lm(age ~ bmi))
summary(fit)

# Rerun with seed 123
imp <- mice(nhanes, method = "norm.nob", m = 1, maxit = 1, seed = 123)
fit <- with(imp, lm(age ~ bmi))
summary(fit)

# Multiple imputation
imp <- mice(nhanes)
imp
attributes(imp)
# Original data
imp$data
# Inputations stored:
imp$imp
c3 <- complete(imp, 3) 
md.pattern(c3)
# Export imputed dataset in long format
c.long <- complete(imp, "long")  
c.long
# Export all 5 imputations in broad format
c.broad <- complete(imp, "broad")
c.broad

# VIGNETTE 2: Algorothmic convergence and inference pooling
# Vary the number of imputations
imp <- mice(nhanes, m = 3, print=F)

# Change the matrix predictor
# Predictor matrix is a square matrix that specifies the variables that are used to impute each incomplete variable
# Look at predictor matrix
imp$pred

# change predictor matrix
ini <- mice(nhanes, maxit=0, print=F)
pred <- ini$pred
pred
pred[ ,"hyp"] <- 0
pred
imp <- mice(nhanes, pred=pred, print=F)

# Quick selection of predictors, auto-selecting predictors according to data relations with a min correlation of .3
ini <- mice(nhanes, pred=quickpred(nhanes, mincor=.3), print=F)
ini$pred

# Inspect the convergence of algorithm
# In general, we would like the streams to intermingle and be free of any trends at the later iterations.
imp <- mice(nhanes, print = F)
plot(imp)

# Change the imputation method
imp$meth
# "pmm" stands for predictive mean matching - default in mice for numerical and integer data
# In reality data is described by a mix of numerical and categorical data
summary(nhanes2)
str(nhanes2)
imp <- mice(nhanes2, print = F)
imp$method
# "logreg" is imputed by logistic regression
# All mice methods
methods(mice)

# Change imputation method for BMI to bayesian normal linear regression imputation
ini <- mice(nhanes2, maxit = 0)
meth <- ini$method
meth
meth["bmi"] <- "norm"
meth
imp <- mice(nhanes2, method = meth, print = F)
plot(imp)

# Extend number of interation
imp40 <- mice.mids(imp, maxit = 35, print = F)
plot(imp40)

# Further diagnostic checking
# Imputed data needs to have the same distribution as observed data
stripplot(imp, chl~.imp, pch = 20, cex = 2)
stripplot(imp)

# Repeated analysis in Mice
fit <- with(imp, lm(bmi~chl))
fit
# Fit contains regression summaries for each data set
class(fit)
# Fit contains mira class
ls(fit)
# Regression model to find 2nd imputed dataset
summary(fit$analyses[[2]])
# Pool analyses from object fit
pool.fit <- pool(fit)
summary(pool.fit)

# VIGNETTE 3: Imputation & nonresponse models
head(boys)
nrow(boys)
summary(boys)
md.pattern(boys)
# How many patterns for which variable gen is missing
mpat <- md.pattern(boys)
sum(mpat[,"gen"] == 0)
# 8 patterns for 503 missing values

# Does the missing data of gen depend on age?
R <- is.na(boys$gen)
R
histogram(~ gen, data = boys)
# Conditional histogram of age given R
histogram(~age|R, data=boys)
# Histogram shows that missingness in gen is not equally distributed across age

# Impute boys dataset with mice
imp1 <- mice(boys, print = F)
# Compare means of imputed with original dataset
summary(boys)
summary(complete(imp1))
# Using summary not practical with lots of variables; to obtain summaries for each imputed value only:
summary(with(imp1, mean(tv)))

# Importance of imputation model
help("mammalsleep")
head(mammalsleep)
summary(mammalsleep)
str(mammalsleep)
md.pattern(mammalsleep)
# Generate 5 imputed datasets with default method pmm, 10 interations
imp <- mice(mammalsleep, maxit = 10)
# Inspect trace lines
plot(imp)
# Perform regression on imputed dataset
fit1 <- with(imp, lm(sws ~ log10(bw) + odi))
pool(fit1)
summary(pool(fit1))
# FMI and Lambda too high due to species being included in the imputation model, there are 62 species and mice auto converts factors
# to dummy varaibles, each species is modeled by its own imputation model
# Impute mammal sleep again, exclude species from data
impnew <- mice(mammalsleep[,-1], maxit = 10)
fit2 <- with(impnew, lm(sws ~log10(bw) + odi))
pool(fit2)
summary(pool(fit2)) # FMI & lambda dramatically decreased, imputation improved
plot(impnew)
# Even though the fraction of information missing due to nonresponse (fmi) and the relative increase in variance due to nonresponse (lambda) are nice and low,
# the convergence turns out to be a real problem. Reason is str of the data
# Total sleep is the sum of paradoxical sleep(ps) and short wave sleep (SWS)
# Relationship was ignored in imputations and needs to be taken into account

# VIGNETTE 4: Passive Imputation and Post-processing
# In the case of incomplete data, one can impute the orginal and trans the completed orginal afterwards
# Or transform incomplete original and impute transformed version
# Goal of passive imputation is to maintain consistency amoung different transformations of the same data

# Use passive imputation to impute deterministic sleep raltion in data
ini <- mice(mammalsleep[,-1], maxit = 0, print = F)
meth <- ini$meth
meth
pred <- ini$pred
pred
pred[c("sws", "ps"), "ts"] <- 0
pred
meth["ts"] <- "~ I(sws + ps)"
pas.imp <- mice(mammalsleep[,-1], method = meth, predictorMatrix = pred, maxit = 10, seed = 123, print = F)
plot(pas.imp)

# Post processing of imputations
ini <- mice(boys, maxit = 0)
meth <- ini$method
meth["tv"] <- "norm"
post <- ini$post
post["tv"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(1,25))"
imp <- mice(boys, method = meth, post = post, print = F)
# Imputed values of tv are now squeezed / constrained to be between 1 and 25
# Compate imputed values and histograms of tv for solution obtained by pmm to the constrained solution
imp.pmm <- mice(boys, printFlag = F)
table(complete(imp)$tv)
table(complete(imp.pmm)$tv)
densityplot(imp, ~tv)
tv <- c(complete(imp.pmm)$tv, complete(imp)$tv)
method <- rep(c("pmm", "norm"), each = nrow(boys))
tvm <- data.frame(tv = tv, method = method)
histogram(~tv|method, data = tvm, nint = 25)

# VIGNETTE 5: Imputing Multi-Level Data
install.packages("pan")
con <- url("https://www.gerkovink.com/mimp/popular.RData")
load(con)
ls()
head(popNCR)
dim(popNCR)
summary(popNCR)
md.pattern(popNCR)
md.pattern(popNCR[,-5])
histogram(~ popteach | is.na(popular), data=popNCR)
