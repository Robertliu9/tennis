if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

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
# Remove variables not needed for training of data
logitTrain <- training %>%
  select(-c(tourney_id, tourney_name, tourney_level, tourney_date, best_of, minutes,
            round, name.p0, name.p1, hand.p0, hand.p1, surface))

# Train model  
logitModel <- train(winner ~ .,
                    data = logitTrain,
                    method = 'glm',
                    na.action = NULL)

summary(logitModel)
# All significant variables are match stats

# Predict on testing set
logitPred <- predict(logitModel, newdata = testing %>%
                       select(-c(tourney_id, tourney_name, tourney_level, tourney_date, 
                                 round, name.p0, name.p1, hand.p0, hand.p1, surface)))

summary(logitPred)
# Compare values
logitCorrect <- (logitPred == testing$winner)
table(logitCorrect)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "2A: Logistic Regression with Stats", 
                                 Accuracy = sum(logitCorrect) / length(logitCorrect)))
accuracy %>% knitr::kable()

# Predicting winners from match stats is relatively simple
# Models need to use values prior to match to predict values
# 3% Improvement

# Predict on training data
logitTrainPred <- predict(logitModel, newdata = training %>%
                       select(-c(tourney_id, tourney_name, tourney_level, tourney_date, 
                                 round, name.p0, name.p1, hand.p0, hand.p1, surface)))
# Compare values
logitTrainCorrect <- (logitTrainPred == training$winner)
table(logitTrainCorrect)

# Baseline training values; if a player is has a lower rank predict win
baselineTrainModel <- training %>%
  select(winner, rank.p0, rank.p1) %>%
  filter(!is.na(rank.p0) & !is.na(rank.p1)) %>%
  mutate(baselinePred = as.integer(ifelse(rank.p0 < rank.p1, 0, 1))) %>%
  mutate(correct = ifelse(baselinePred == winner, 1, 0))

table(baselineTrainModel$correct)

# Model 3 - CART Model
# Remove variables not needed for training of data
cartTrain <- training %>%
  select(-c(tourney_id, tourney_name, tourney_level, tourney_date, best_of, minutes,
            round, name.p0, name.p1, hand.p0, hand.p1, surface))

# Train model  
cartModel <- train(winner ~ .,
                    data = logitTrain,
                    method = 'rpart')

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

# Model 5 - Multilayer Perceptron Network by Stocastic Gradient Descent

# Model 6 - PCA



# Connect with DNA 
%>%
  filter(winner_name %in% dna$player & loser_name %in% dna$player) %>%
  left_join(dna, by = c("name.p1" = "player")) %>%
  left_join(dna, by = c("name.p1" = "player"), suffix = c(".p1", ".p2"))

