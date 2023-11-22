install.packages("dplyr")

library(dplyr)


df = read.csv(unzip('Data/MensODIDataUnique-csv.zip'))

df <- df[ , 2:ncol(df)]

df <- df %>% arrange(MatchNumber, Inning, over, bowl_nbr)


#changing datatypes
df$Gender <- factor(df$Gender)
df$MatchDate <- as.POSIXlt(df$MatchDate, format = '%Y-%m-%d')
factorcol <- c('Location', 'Stadium', 'TossWinner', 'TossWinnerDecision', 
               'BattingTeam', 'BowlingTeam', 'Inning', 'Sbatter', 'NSbatter', 
               'bowler', 'extra_type', 'playerDismissed', 
               'dismissalKind', 'fielder', 'Winner', 'MoM') 
df[ , factorcol] <- lapply(df[ , factorcol], FUN = factor)


#creating new field
df$overType <- ifelse(df$over <= 10, 'P1',
                      ifelse(df$over %in% 11:40, 'P2', 'P3'))

#OR
df$overType = case_when(
  df$over <= 10 ~ "P1",
  df$over %in% 11:40 ~ "P2",
  df$over > 40 ~ "P3"
)

df$isLegalBowl <- case_when (
  df$extra_type %in% c('noballs', 'wides') ~ 0, 
  TRUE ~ 1
)


df$overType <- factor(df$overType)



##Countrywise number of wickets taken in given powerplay since given date
#Taking input
ppWicketTaken <- function(sinceDate = '2010-01-01', powerPlay = 'P1') {
  
  #sinceDate <- '2010-01-01'
  sinceDate <- as.POSIXct(sinceDate)
  #powerPlay <- 'P2'
  
  ppWicketTakendf <- df %>% filter(overType == powerPlay, MatchDate >= sinceDate) %>%
    group_by(BowlingTeam) %>%
    summarise(wicketsTaken = sum(is_wicket), 
              noOfMatches = length(unique(MatchNumber)), 
              wicketRate = wicketsTaken/noOfMatches) %>%
    arrange(desc(wicketRate))
  return(ppWicketTakendf) 
}


##Countrywise number of wickets lost in given powerplay since given date
ppWicketLost <- function(sinceDate = '2010-01-01', powerPlay = 'P1') {
  
  #sinceDate <- '2010-01-01'
  sinceDate <- as.POSIXct(sinceDate)
  #powerPlay <- 'P2'
  
  ppWicketLostdf <- df %>% filter(overType == powerPlay, MatchDate >= sinceDate) %>%
    group_by(BattingTeam) %>%
    summarise(wicketsLost = sum(is_wicket), 
              noOfMatches = length(unique(MatchNumber)), 
              wicketRate = wicketsLost/noOfMatches) %>%
    arrange(desc(wicketRate))
  return(ppWicketLostdf) 
}


##Runrate of batting team in given powerplay since given date
ppRunRate <- function(sinceDate = '2010-01-01', powerPlay = 'P1') {
  
  #sinceDate <- '2010-01-01'
  sinceDate <- as.POSIXct(sinceDate)
  #powerPlay <- 'P2'
  
  ppRunRatedf <- df %>% filter(overType == powerPlay, MatchDate >= sinceDate) %>%
    group_by(BattingTeam) %>%
    summarise(totalppRuns = sum(totalRuns),
              totalBowls = sum(isLegalBowl),
              runRate = totalppRuns*6/totalBowls) %>%
    arrange(desc(runRate))
  return(ppRunRatedf) 
}



ppWicketTakendf <- ppWicketTaken('2012-01-01', 'P1')
ppWicketLostdf <- ppWicketLost('2012-01-01', 'P1')
ppRunRatedf <- ppRunRate('2020-01-01', 'P3')



#Calculating cumulative runs, wickets, Cumulative partnership, player runs etc

#Cumulative team runs and wickets
tempdf <- df %>% arrange(MatchNumber, Inning, over, bowl_nbr) %>%
  group_by(MatchNumber, Inning) %>% 
  summarise(cumTotalRuns = cumsum(totalRuns),
            cumTotalBowls = cumsum(isLegalBowl),
            cumWickets = cumsum(is_wicket))

df <- cbind(df, tempdf[,c('cumTotalRuns', 'cumTotalBowls', 'cumWickets')])



#Calculating partnership
tempdf <- df %>% arrange(MatchNumber, Inning, over, bowl_nbr) %>%
  group_by(MatchNumber, Inning, cumWickets) %>%
  summarise(partnershipRun = cumsum(totalRuns),
            partnershipBowl = cumsum(isLegalBowl))


df <- cbind(df, tempdf[ , c("partnershipRun", "partnershipBowl")])


##Creating field CRR to calculate Current Run Rate at any given point.
df$CRR <- (df$cumTotalRuns*6)/df$cumTotalBowls
df$partnershipRR = (df$partnershipRun*6)/df$partnershipBowl



##Beginning Predictor Model
MLdf <- df[(df$Inning == 1 & df$Gender == 'male') , 
           c(2, 5, 9, 10, 12, 13, 31, 32, 33, 34, 35, 36, 37) ]

runScored1 = MLdf %>% group_by(MatchNumber) %>% summarise(runScored = max(cumTotalRuns))
MLdf = merge(MLdf, runScored1, by = intersect(names(MLdf), names(runScored1)))

#MLdf's CRR and Partnership run rate has Infinity as values. Replacing both with 4.5
MLdf[(MLdf$CRR == Inf) , 'CRR'] = 4.5
MLdf[(MLdf$partnershipRR == Inf) , 'partnershipRR'] = 4.5


#Creating basic linear regression model
CricketScoreModel = lm(runScored ~ ., data = MLdf)


x <- summary(CricketScoreModel)$coefficients

x <- cbind(MLdf, CricketScoreModel$fitted.values)

















