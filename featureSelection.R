library(caret)
if(!require(pbapply)) install.packages("pbapply", repos = "http://cran.us.r-project.org")
if(!require(sjstats)) install.packages("sjstats", repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")

# Remove unnecessary variables & Convert factors
atp <- atp %>%
  select(-c(draw_size, match_num, winner_id, winner_seed, winner_entry, winner_ioc, loser_id, loser_seed, loser_entry, loser_ioc, 
            w_SvGms, l_SvGms, winner_ht, loser_ht, score)) %>%
  mutate_at(vars(surface, tourney_level, winner_hand, loser_hand, round, best_of), as.factor)

# Inspect Data
summary(atp)

# Adjust factors with no values to unknown values
atp$surface[which(atp$surface == "")] <- "None"
atp$surface <- factor(atp$surface)
atp$winner_hand[which(atp$winner_hand == "")] <- "U"
atp$winner_hand <- factor(atp$winner_hand)
atp$loser_hand[which(atp$loser_hand == "")] <- "U"
atp$loser_hand <- factor(atp$loser_hand)

# Impute missing data
# Inspect missing data
md.pattern(atp, rotate.names = T)
# Looks like when match stats are not available there is no stats at all

# Remove NA's
atp <- na.omit(atp)

# Set seed
set.seed(17)

# Using dataset, replace winner & losers with Player 0 or 1 by random. Record who won in another column
# Ensure all stats assigned to players are recorded by the correct column rows
# Remove Height, which has lots of na's
atp <- atp %>%
  mutate(winner = as.factor(sample(0:1, n(), replace = T))) %>%
  mutate(name.p0 = if_else(winner == 0, winner_name, loser_name),
         name.p1 = if_else(winner == 0, loser_name, winner_name),
         age.p0 = if_else(winner == 0, winner_age, loser_age),
         age.p1 = if_else(winner == 0, loser_age, winner_age),
         hand.p0 = if_else(winner == 0, winner_hand, loser_hand),
         hand.p1 = if_else(winner == 0, loser_hand, winner_hand),
         aceRate.p0 = if_else(winner == 0, w_ace/w_svpt, l_ace/l_svpt),
         aceRate.p1 = if_else(winner == 0, l_ace/l_svpt, w_ace/w_svpt),
         dfRate.p0 = if_else(winner == 0, w_df/w_svpt, l_df/l_svpt),
         dfRate.p1 = if_else(winner == 0, l_df/l_svpt, w_df/w_svpt),
         fSrvInRate.p0 = if_else(winner == 0, w_1stIn/w_svpt, l_1stIn/l_svpt),
         fSrvInRate.p1 = if_else(winner == 0, l_1stIn/l_svpt, w_1stIn/w_svpt),
         fSrvWonRate.p0 = if_else(winner == 0, w_1stWon/w_1stIn, l_1stWon/l_1stIn),
         fSrvWonRate.p1 = if_else(winner == 0, l_1stWon/l_1stIn, w_1stWon/w_1stIn),
         SSrvWonRate.p0 = if_else(winner == 0, w_2ndWon/(w_svpt-w_1stIn), l_2ndWon/(l_svpt-l_1stIn)),
         SSrvWonRate.p1 = if_else(winner == 0, l_2ndWon/(l_svpt-l_1stIn), w_2ndWon/(w_svpt-w_1stIn)),
         bpSavedRate.p0 = if_else(winner == 0, w_bpSaved/w_bpFaced, l_bpSaved/l_bpFaced),
         bpSavedRate.p1 = if_else(winner == 0, l_bpSaved/l_bpSaved, w_bpSaved/w_bpSaved),
         rankPts.p0 = if_else(winner == 0, winner_rank_points, loser_rank_points),
         rankPts.p1 = if_else(winner == 0, loser_rank_points, winner_rank_points),
         rank.p0 = if_else(winner == 0, winner_rank, loser_rank),
         rank.p1 = if_else(winner == 0, loser_rank, winner_rank)) %>%
  select(-c(winner_name, winner_hand, winner_age, 
            loser_name, loser_hand, loser_age,
            w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_bpSaved, w_bpFaced, winner_rank_points, winner_rank,
            l_ace, l_df, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_bpSaved, l_bpFaced, loser_rank_points, loser_rank))

# Court type win rate
# Games, wins, win% by court type
courtWinRate <- atp %>%
  select(name.p0, name.p1, surface, tourney_date, winner) %>%
  gather(key = label, value = name, -c(surface, tourney_date, winner)) %>%
  mutate(win = str_extract(label, '\\d') == winner)

# Create new variables giving players winning % for the last 3 months and winning percentage on court surface for the last 3 years 
atp <- atp %>%
  rowwise() %>%
  mutate(surfaceWinRate.p0 = sum((courtWinRate$name == name.p0) & 
                                (surface == courtWinRate$surface) & 
                                ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date)) &
                                (courtWinRate$win == TRUE)) /
                             sum((courtWinRate$name == name.p0) & 
                                (surface == courtWinRate$surface) & 
                                ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date))), 
         surfaceWinRate.p1 = sum((courtWinRate$name == name.p1) & 
                                (surface == courtWinRate$surface) & 
                                ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date)) &
                                (courtWinRate$win == TRUE)) /
                             sum((courtWinRate$name == name.p1) & 
                                (surface == courtWinRate$surface) & 
                                ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date))),
         form.p0 = sum((courtWinRate$name == name.p0) &
                         (courtWinRate$win == TRUE) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))) /
                   sum((courtWinRate$name == name.p0) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))),
         form.p1 = sum((courtWinRate$name == name.p1) &
                         (courtWinRate$win == TRUE) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))) /
                   sum((courtWinRate$name == name.p1) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))))

# Check to see if values make sense
atp %>% select(tourney_date, name.p0, name.p1, surfaceWinRate.p0, surfaceWinRate.p1, form.p0, form.p1) %>%
  filter(tourney_date > 20141001) %>%
  filter(name.p0 == "Roger Federer") %>%
  slice(1:20)

# Create Fatigue Dataframe
fatigue <- atp %>%
  select(name.p0, name.p1, tourney_date, minutes) %>%
  gather(key = label, value = name, -c(tourney_date, minutes))

# Get fatigue for player from last month
atp <- atp %>%
  rowwise() %>%
  mutate(fatigue.p0 = sum(fatigue$minutes[((fatigue$name == name.p0) &
                                             ((fatigue$tourney_date < tourney_date) & (fatigue$tourney_date + 100 > tourney_date)))]),
         fatigue.p1 = sum(fatigue$minutes[((fatigue$name == name.p1) &
                                             ((fatigue$tourney_date < tourney_date) & (fatigue$tourney_date + 100 > tourney_date)))]))

# Check data makes sense
atp %>% select(tourney_date, name.p0, name.p1, fatigue.p0, fatigue.p1, minutes) %>%
  filter(name.p0 == "Andy Murray" | name.p1 == "Andy Murray") %>%
  filter(tourney_date > 20141001) %>%
  slice(1:20)

# Head to Head Balance
h2h <- atp %>%
  select(name.p0, name.p1, winner, tourney_date) %>%
  mutate(win = str_extract("name.p0", '\\d') == winner)

# For every match find culmulative head to head to that date
# Negative values give p1 the advantage
atp <- atp %>% rowwise() %>%
  mutate(h2h = 2*(sum((((h2h$name.p0 == name.p0) & (h2h$name.p1 == name.p1)) &
                        (h2h$tourney_date < tourney_date) &
                        (h2h$winner == 0)) |
                        (((h2h$name.p0 == name.p1) & (h2h$name.p1 == name.p0)) &
                        (h2h$tourney_date < tourney_date) &
                        (h2h$winner == 1)))) -
               sum((((h2h$name.p0 == name.p0) & (h2h$name.p1 == name.p1)) |
                    ((h2h$name.p0 == name.p1) & (h2h$name.p1 == name.p0))) &
                     (h2h$tourney_date < tourney_date)))

# Check a h2h matchup to ensure accuracy of function
atp %>% select(tourney_date, name.p0, name.p1, winner, h2h) %>%
  filter(((name.p0 == "Roger Federer") & (name.p1 == "Rafael Nadal")) |
         ((name.p1 == "Roger Federer") & (name.p0 == "Rafael Nadal"))) %>%
  slice(1:20)

# Stats Record
stats <- atp %>%
  select(name.p0, name.p1, tourney_date, aceRate.p0, aceRate.p1, dfRate.p0, dfRate.p1, 
         fSrvInRate.p0, fSrvInRate.p1, fSrvWonRate.p0, fSrvWonRate.p1,
         SSrvWonRate.p0, SSrvWonRate.p1, bpSavedRate.p0, bpSavedRate.p1) %>%
  gather(key = label, value = name, c(name.p0, name.p1)) %>%
  mutate(aceRate = if_else(str_extract(label, '\\d') == 0, aceRate.p0, aceRate.p1),
         dfRate = if_else(str_extract(label, '\\d') == 0, dfRate.p0, dfRate.p1),
         fSrvInRate = if_else(str_extract(label, '\\d') == 0, fSrvInRate.p0, fSrvInRate.p1),
         fSrvWonRate = if_else(str_extract(label, '\\d') == 0, fSrvWonRate.p0, fSrvWonRate.p1),
         SSrvWonRate = if_else(str_extract(label, '\\d') == 0, SSrvWonRate.p0, SSrvWonRate.p1),
         bpSavedRate = if_else(str_extract(label, '\\d') == 0, bpSavedRate.p0, bpSavedRate.p1)) %>%
  select(-c(aceRate.p0, aceRate.p1, dfRate.p0, dfRate.p1, fSrvInRate.p0, fSrvInRate.p1,
            fSrvWonRate.p0, fSrvWonRate.p1, SSrvWonRate.p0, SSrvWonRate.p1, bpSavedRate.p0, bpSavedRate.p1,
            label))

summary(stats)  
# Na's are from dividing by 0, replace with 0
stats[is.na(stats)] <- 0
summary(stats)

# Add stats for the players from the last two year
atp <- atp %>% rowwise() %>%
  mutate(aceRate.p0 = sum(stats$aceRate[((stats$name == name.p0) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         aceRate.p1 = sum(stats$aceRate[((stats$name == name.p1) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         dfRate.p0 = sum(stats$dfRate[((stats$name == name.p0) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         dfRate.p1 = sum(stats$dfRate[((stats$name == name.p1) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvInRate.p0 = sum(stats$fSrvInRate[((stats$name == name.p0) & 
                                         ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvInRate.p1 = sum(stats$fSrvInRate[((stats$name == name.p1) & 
                                         ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvWonRate.p0 = sum(stats$fSrvWonRate[((stats$name == name.p0) & 
                                                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvWonRate.p1 = sum(stats$fSrvWonRate[((stats$name == name.p1) & 
                                                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         SSrvWonRate.p0 = sum(stats$SSrvWonRate[((stats$name == name.p0) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         SSrvWonRate.p1 = sum(stats$SSrvWonRate[((stats$name == name.p1) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         bpSavedRate.p0 = sum(stats$bpSavedRate[((stats$name == name.p0) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p0) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         bpSavedRate.p1 = sum(stats$bpSavedRate[((stats$name == name.p1) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
                        sum((stats$name == name.p1) &
                              ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))))

# Check stats to ensure accuracy of function
atp %>% select(tourney_date, name.p0, aceRate.p0, dfRate.p0, fSrvInRate.p0, fSrvWonRate.p0, SSrvWonRate.p0, bpSavedRate.p0) %>%
  filter(name.p0 == "Roger Federer") %>%
  filter(tourney_date > 20110000) %>%
  slice(1:20)

# Remove unneeded dataframes
rm(courtWinRate, fatigue, h2h, stats)

# Look at Na values calculated
summary(atp)

# Convert Na's to 0
atp[is.na(atp)] <- 0
summary(atp)

# Use 2019 for validation set
validation <- atp %>%
  filter(tourney_date > 20190000)

# Use 2018 for test set
testing <- atp %>%
  filter(tourney_date > 20180000 & tourney_date < 20190000)

# Any matches before 2017 is inital training data
training <- atp %>%
  filter(tourney_date < 20180000)
