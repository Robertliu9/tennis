if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(RSNNS)) install.packages("RSNNS", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")

# Model 1 - Baseline; if a player is has a lower rank predict win
baselineModel <- testing %>%
  select(winner, rank.p0, rank.p1) %>%
  filter(!is.na(rank.p0) & !is.na(rank.p1)) %>%
  mutate(baselinePred = as.integer(ifelse(rank.p0 < rank.p1, 0, 1))) %>%
  mutate(correct = ifelse(baselinePred == winner, 1, 0))

table(baselineModel$correct)
accuracy <- data_frame(Method = "baseline", Accuracy = mean(baselineModel$correct))
accuracy %>% knitr::kable()

# Graph showing %correct by difference in rank
baselineModel %>%
  mutate(rankdiff = abs(rank.p0 - rank.p1)) %>%
  group_by(grp = cut(rankdiff, breaks = c(0,2,6,12,20,30,45,65,100,150))) %>%
  summarise(perCorrect = sum(correct)/n(),
            games = n()) %>%
  ggplot(aes(grp, perCorrect)) +
  geom_point(aes(size = games)) +
  scale_size(range = c(0,3)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Baseline Model - % correct by difference in Rank",
       x = "Difference in Rank grouping",
       y = "% of correct predictions")

# Look at accuracy of training data using baseline model
# Split by year
dates <- seq(20110000, 20180000, 10000)

baselineWinsByYear <- pbsapply(dates, function(d){
  
  yearWins <- training %>%
    filter(tourney_date > (d-10000) & tourney_date < d) %>%
    select(winner, rank.p0, rank.p1) %>%
    filter(!is.na(rank.p0) & !is.na(rank.p1)) %>%
    mutate(baselinePred = as.integer(ifelse(rank.p0 < rank.p1, 0, 1))) %>%
    mutate(correct = ifelse(baselinePred == winner, 1, 0))
    
  return(mean(yearWins$correct))
  
})

# Graph showing rate of baseline correct by year
baselineWinsByYear <- as.data.frame(baselineWinsByYear)
rownames(baselineWinsByYear) <- seq(2010,2017,1)
colnames(baselineWinsByYear) <- "correct"
knitr::kable(baselineWinsByYear)
baselineWinsByYear %>% ggplot(aes(x = rownames(baselineWinsByYear), y = correct)) +
  geom_point() +
  scale_y_continuous(limits = c(0,1)) +
  annotate("text", x = 2, y = c(0.5, 0.45),
           label = c(paste0("St. Dev: ", round(sd(baselineWinsByYear$correct),3)),
                     paste0("CoV: ", round(cv(baselineWinsByYear$correct), 3)))) +
  labs(title = "Baseline Model - % correct by year",
       x = "Year",
       y = "% of correct predictions")
  

# Between 2010 & 2018, difference in rank gives a more accurate prediction of winner
baselineModel %>%
  mutate(minRank = pmin(rank.p0, rank.p1)) %>%
  group_by(grp = cut(minRank, breaks = c(0,2,6,12,20,30,45,65,100,150))) %>%
  summarise(perCorrect = sum(correct)/n(),
            games = n()) %>%
  ggplot(aes(grp, perCorrect)) +
  geom_point(aes(size = games)) +
  scale_size(range = c(0,3)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Baseline Model - % correct by Player Rank",
       x = "Player Rank grouping",
       y = "% of correct predictions")

# Model 2A - Logistic Regression

# Train model  
logitModel <- caret::train(winner ~ . -tourney_date,
                    data = training,
                    method = 'glm',
                    family = "binomial",
                    na.action = NULL)

summary(logitModel)
# All significant variables are match stats

# Predict on testing set
logitPred <- predict(logitModel, newdata = testing)

summary(logitPred)
# Compare values
logitCorrect <- (logitPred == testing$winner)
table(logitCorrect)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "2A: Logistic Regression with Stats", 
                                 Accuracy = sum(logitCorrect) / length(logitCorrect)))
accuracy %>% knitr::kable()

table(logitPred == testing$winner, testing$winner)

# 244 correct upset predictions
correctUpsets <- testing[logitPred == testing$winner & 
          ((testing$winner == 0 & testing$rank.p0 > testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 > testing$rank.p0)),]

# 172 incorrect upset predictions
wrongUpsets <- testing[logitPred != testing$winner & 
                  ((testing$winner == 0 & testing$rank.p0 < testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 < testing$rank.p0)),]

# 1678 correct favourite predictions
correctFav <- testing[logitPred == testing$winner & 
                  ((testing$winner == 0 & testing$rank.p0 < testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 < testing$rank.p0)),]

# 804 incorrect favourite predictions
wrongFav <- testing[logitPred != testing$winner & 
                        ((testing$winner == 0 & testing$rank.p0 > testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 > testing$rank.p0)),]


# Predicting winners from match stats is relatively simple
# Models need to use values prior to match to predict values
# 3% Improvement

# Model 2B - Logistic Regression remove more variables
# Train model  
logitModel2B <- caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
                             - fSrvInRate.p0 - fSrvInRate.p1,
                           data = training,
                           method = 'glm',
                           na.action = NULL)

summary(logitModel2B)

# Predict on testing set
logitPred2B <- predict(logitModel2B, newdata = testing)

summary(logitPred2B)
# Compare values
logitCorrect2B <- (logitPred2B == testing$winner)
table(logitCorrect2B)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "2B: Logistic Regression no serve Stats", 
                                 Accuracy = sum(logitCorrect2B) / length(logitCorrect2B)))
accuracy %>% knitr::kable()

# Predict on training data???
logitTrainPred <- predict(logitModel, newdata = training %>%
                       select(-c(tourney_id, tourney_name, tourney_level, tourney_date, 
                                 round, name.p0, name.p1, hand.p0, hand.p1, surface)))
# Compare values
logitTrainCorrect <- (logitTrainPred == training$winner)
table(logitTrainCorrect)

# Model 3 - CART Model
# Remove variables not needed for training of data
cartTrain <- training %>%
  select(-c(tourney_id, tourney_name, tourney_level, tourney_date, best_of, minutes,
            round, name.p0, name.p1, hand.p0, hand.p1, surface))

# Train model  
cartModel <- train(winner ~ .,
                    data = cartTrain,
                    method = 'rpart',
                    cp = 0.9,
                    minsplit = 5,
                    minbucket = 6)

plot(cartModel$finalModel)
text(cartModel$finalModel)

# Predict on testing set
cartPred <- predict(cartModel, newdata = testing %>%
                       select(-c(tourney_id, tourney_name, tourney_level, tourney_date, 
                                 round, name.p0, name.p1, hand.p0, hand.p1, surface)))

summary(cartPred)
# Compare values
cartCorrect <- (cartPred == testing$winner)
table(cartCorrect)

# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "3: CART Model", 
                                 Accuracy = sum(cartCorrect) / length(cartCorrect)))
accuracy %>% knitr::kable()

# Model 4 - Random Forest Model
# Remove variables not needed for training of data
rfTrain <- training %>%
  select(-c(tourney_id, tourney_name, tourney_level, tourney_date, best_of, minutes,
            round, name.p0, name.p1, hand.p0, hand.p1, surface))

# Train model
trainctrl <- trainControl(verboseIter = TRUE)
rfModel <- train(winner ~ .,
                 data = rfTrain,
                 method = 'Rborist',
                 trControl = trainctrl)

# Predict on testing set
rfPred <- predict(rfModel, newdata = testing %>%
                      select(-c(tourney_id, tourney_name, tourney_level, tourney_date, 
                                round, name.p0, name.p1, hand.p0, hand.p1, surface)))

summary(rfPred)
# Compare values
rfCorrect <- (rfPred == testing$winner)
table(rfCorrect)

# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "4: Random Forest Model", 
                                 Accuracy = sum(rfCorrect) / length(rfCorrect)))
accuracy %>% knitr::kable()

# Model 5 - Ensemble Models
models <- c("glm", "lda", "naive_bayes", "svmLinear", "gamLoess", "multinom", "rf")

fits <- pblapply(models, function(model){ 
  print(model)
  caret::train(winner ~ . - tourney_date, method = model, data = training)
})

names(fits) <- models

pred <- pbsapply(fits, function(object) 
  predict(object, newdata = testing))
dim(pred)

accEnsem <- colMeans(pred == testing$winner)
accEnsem
mean(accEnsem)

votes <- rowMeans(pred == "0")
y_hat <- ifelse(votes > 0.5, "0", "1")

accuracy <- bind_rows(accuracy,
                      data_frame(Method = "5: Ensemble Models", 
                                 Accuracy = mean(y_hat == testing$winner)))
accuracy %>% knitr::kable()


# Model 6 - Multilayer Perceptron Network by Stocastic Gradient Descent
# Pre processing of data - Normalise data using min-max method
trainingIndex <- ifelse(atp$tourney_date < 20180000, 1, 0)
testingIndex <- ifelse(atp$tourney_date > 20180000 & atp$tourney_date < 20190000, 1, 0)
validationIndex <- ifelse(atp$tourney_date > 20190000, 1, 0)
sum(trainingIndex, testingIndex, validationIndex)
maxs <- apply(atp %>% select(-c(winner, tourney_date)), 2, max)
mins <- apply(atp %>% select(-c(winner, tourney_date)), 2, min)
scaledAtp <- as.data.frame(scale(atp %>% select(-c(winner, tourney_date)), 
                                 center = mins,
                                 scale = maxs - mins))
summary(scaledAtp)
nnTrain <- scaledAtp[trainingIndex,]
nnTrain$winner <- training$winner
nnTest <- scaledAtp[testingIndex,]
nnTest$winner <- testing$winner
nnValidation <- scaledAtp[validationIndex,]
nnValidation$winner <- validation$winner

# Train NN with 2 layers of 2 neurons
nnModel <- neuralnet(winner ~.,
                     data = nnTrain,
                     hidden = c(10,6),
                     linear.output = TRUE,
                     threshold = .002)

nnModel$result.matrix
plot(nnModel)

# Create prediction with model
nnPred <- compute(nnModel, nnTest[1:25])
str(nnPred)
table(nnPred$net.result)

# Model 6B - Multilayer Perceptron Network by Stocastic Gradient Descent
# Pre processing of data - Normalise data using min-max method
trainingIndex <- ifelse(atp$tourney_date < 20180000, 1, 0)
testingIndex <- ifelse(atp$tourney_date > 20180000 & atp$tourney_date < 20190000, 1, 0)
validationIndex <- ifelse(atp$tourney_date > 20190000, 1, 0)
sum(trainingIndex, testingIndex, validationIndex)
maxs <- apply(atp %>% select(-tourney_date) %>%
                mutate(winner = as.numeric(winner)), 2, max)
mins <- apply(atp %>% select(-tourney_date) %>%
                mutate(winner = as.numeric(winner)), 2, min)
scaledAtp <- as.data.frame(scale(atp %>% select(-tourney_date) %>%
                                   mutate(winner = as.numeric(winner)), 
                                 center = mins,
                                 scale = maxs - mins))
summary(scaledAtp)
nnTrain <- scaledAtp[trainingIndex == 1,]
nnTest <- scaledAtp[testingIndex == 1,]
nnValidation <- scaledAtp[validationIndex == 1,]

# Train NN with 2 hidden layers of 10 and 6 neurons
set.seed(15, sample.kind = 'Rounding')
nnModel <- neuralnet(winner ~ .,
                     data = nnTrain,
                     hidden = c(5,3),
                     linear.output = F,
                     threshold = .01,
                     learningrate = 0.02,
                     algorithm = "rprop+",
                     act.fct = "tanh",
                     stepmax = 1000000)

nnModel$result.matrix
plot(nnModel)

# Create prediction with model
nnPred <- predict(nnModel, nnTest)
str(nnPred)
table(nnPred)
nnPred$net.result
table(nnPred$net.result)

output <- compute(nnModel, nnTest)
summary(output)
prob <- output$net.result
summary(prob)

# Model 7 - PCA
# Complete PCA
pcomp <- prcomp(training %>% select(-c(tourney_date, winner)), scale. = T)
# Mean of variables
pcomp$center

# SD of variables
pcomp$scale

# Look at rotation of first 5 principle compenents
pcomp$rotation[,1:5]

summary(pcomp)

data.frame(pcomp$x[,1:2], type = training$winner) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

data.frame(winner = training$winner, pcomp$x[,1:10]) %>%
  gather(key = "PC", value = "value", -winner) %>%
  ggplot(aes(PC, value, fill = winner)) +
  geom_boxplot()

# Compute variance of each principle component
pvar <- pcomp$sdev^2

# Compute proportion of variance explained
propvar <- pvar / sum(pvar)

# Plot of Variables explained
plot(propvar, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# Cumulative Screen Plot
plot(cumsum(propvar), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# Test data
trainPCA <- as.data.frame(pcomp$x)[,1:13] %>%
  cbind(training$winner) %>% rename(winner = `training$winner`)
testPCA <- as.data.frame(predict(pcomp, testing))[,1:13] %>%
  cbind(testing$winner) %>% rename(winner = `testing$winner`)
validationPCA <- as.data.frame(predict(pcomp, validation))[,1:13] %>%
  cbind(validation$winner) %>% rename(winner = `validation$winner`)

summary(trainPCA)
str(trainPCA)

# Logistic Model using PCA
logitPCA <- caret::train(winner ~ .,
                             data = trainPCA,
                             method = 'glm',
                             na.action = NULL)

summary(logitPCA)
# All significant variables are match stats

# Predict on testing set
logitPCAPred <- predict(logitPCA, newdata = testPCA)

summary(logitPCAPred)
# Compare values
logitPCACorrect <- (logitPCAPred == testing$winner)
table(logitPCACorrect)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Logistic Regression with PCA", 
                                 Accuracy = sum(logitPCACorrect) / length(logitPCACorrect)))
accuracy %>% knitr::kable()

table(logitPCAPred == testing$winner, testing$winner)

# 292 correct upset predictions
nrow(testing[logitPCAPred == testing$winner & 
                ((testing$winner == 0 & testing$rank.p0 > testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 > testing$rank.p0)),])

# 247 incorrect upset predictions
nrow(testing[logitPCAPred != testing$winner & 
                         ((testing$winner == 0 & testing$rank.p0 < testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 < testing$rank.p0)),])

# 1603 correct favourite predictions
nrow(testing[logitPCAPred == testing$winner & 
                        ((testing$winner == 0 & testing$rank.p0 < testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 < testing$rank.p0)),])

# 756 incorrect favourite predictions
nrow(testing[logitPCAPred != testing$winner & 
                      ((testing$winner == 0 & testing$rank.p0 > testing$rank.p1) | (testing$winner == 1 & testing$rank.p1 > testing$rank.p0)),])

# Neural Network
# Pre processing of data - Normalise data using min-max method
trainingIndex <- ifelse(atp$tourney_date < 20180000, 1, 0)
testingIndex <- ifelse(atp$tourney_date > 20180000 & atp$tourney_date < 20190000, 1, 0)
validationIndex <- ifelse(atp$tourney_date > 20190000, 1, 0)
sum(trainingIndex, testingIndex, validationIndex)
nnAtp <- rbind(trainPCA, testPCA, validationPCA)
maxs <- apply(nnAtp %>% mutate(winner = as.numeric(winner)), 2, max)
mins <- apply(nnAtp %>% mutate(winner = as.numeric(winner)), 2, min)
scaledNnAtp <- as.data.frame(scale(nnAtp %>% mutate(winner = as.numeric(winner)), 
                                 center = mins,
                                 scale = maxs - mins))
summary(scaledNnAtp)
nnTrain <- scaledAtp[trainingIndex == 1,]
nnTest <- scaledAtp[testingIndex == 1,]
nnValidation <- scaledAtp[validationIndex == 1,]

# Train NN with 2 hidden layers of 10 and 6 neurons
set.seed(15, sample.kind = 'Rounding')
nnModel <- neuralnet(winner ~ .,
                     data = nnTrain,
                     hidden = 4,
                     linear.output = F,
                     threshold = .01,
                     learningrate = 0.02,
                     algorithm = "rprop+",
                     act.fct = "tanh",
                     stepmax = 1000000)

nnModel$result.matrix
plot(nnModel)

# Create prediction with model
nnPred <- predict(nnModel, nnTest)
str(nnPred)
table(nnPred)
nnPred$net.result
table(nnPred$net.result)

output <- compute(nnModel, nnTest)
summary(output)
prob <- output$net.result
summary(prob)


# Connect with DNA 
%>%
  filter(winner_name %in% dna$player & loser_name %in% dna$player) %>%
  left_join(dna, by = c("name.p1" = "player")) %>%
  left_join(dna, by = c("name.p1" = "player"), suffix = c(".p1", ".p2"))

