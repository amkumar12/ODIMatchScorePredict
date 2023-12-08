install.packages("dplyr")

library(dplyr)
library(lubridate)
library(ggplot2)

df = read.csv(unzip('Data/MensODIDataUnique-csv.zip'))


# Not the right way
# We can simply delete the column by setting it equal to NULL
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
df$matchYear =  lubridate::year(df$MatchDate)
df$oversRemaining = 50 - df$over


#No balls become a headache and may increase run rate quite a bit
#Chcking overall strike rate of players on free hit ball
#df %>% arrange( MatchNumber, Inning, over, bowl_nbr )

#First checking team wise noballs bowled since 2003
# To check if there is any impactof free hit on no balls

noBalls = df[df$extra_type == 'noballs' & !is.na(df$extra_type), 
             c(2,3,9,10,11,12,14,16,17,18,19)]

#Plotting year wise line graph of number of no-balls bowled by each team
noBalls$matchYear = year(noBalls$MatchDate)

noBallsByYear <- noBalls %>% filter(matchYear >= 2003) %>%
  group_by(matchYear, BowlingTeam) %>% 
  summarise(noBallsCount = n(),
            matchCount = length(unique(MatchNumber)),
            noBallsPerMatch = noBallsCount/matchCount)
teams <- c('India', 'New Zealand', 'Australia', 'Bangladesh', 'England', 
           'Pakistan', 'South Africa', 'Sri Lanka', 'West Indies')
noBallsByYear = as.data.frame(noBallsByYear[ noBallsByYear$BowlingTeam %in% teams, c(1,2,5) ])

ggplot(data = noBallsByYear, 
                mapping = aes(x = matchYear, y = noBallsPerMatch, color = BowlingTeam)) +
  geom_line(linewidth = 1.5) +
  xlab('Year') +
  ylab('Noballs per Match') +
  ggtitle('                                                                Impact of Free Hit rule on Noballs bowled by Teams') +
  scale_x_continuous(breaks = seq(2003, 2023, 2)) +
  theme_minimal()




#Now, checking runs scored on free hit balls

colsAgainstNoballs = c('BattingTeam','BowlingTeam','Inning', 'over', 'Sbatter',
                       'bowler','batterRuns','extraRuns', 'totalRuns','extra_type')
againstNoballs = data.frame(matrix(nrow = 0, ncol = length(colsAgainstNoballs)))
for (i in 1:nrow(tempdf)) {
  if(tempdf$extra_type[i] == 'noballs' & !is.na(tempdf$extra_type[i])) {
    row = tempdf[ i+1 , colsAgainstNoballs]
    againstNoballs = rbind(againstNoballs, row)
    i = i+1
    while(tempdf$isLegalBowl[i] == 0 & !is.na(tempdf$extra_type[i])){
      row = tempdf[ i+1 , colsAgainstNoballs]
      againstNoballs = rbind(againstNoballs, row)
      i = i+1
    }
  }
}





##Beginning Predictor Model
MLdf <- df[(df$Inning == 1 & df$Gender == 'male' & df$MatchDate >= 2015-01-01) , 
           c(4, 8, 9, 10, 12, 13, 21, 29, 31, 32, 33, 34, 35, 36, 37) ]

runScored1 = MLdf %>% group_by(MatchNumber) %>% summarise(runScored = max(cumTotalRuns))
MLdf = merge(MLdf, runScored1, by = intersect(names(MLdf), names(runScored1)))

#MLdf's CRR and Partnership run rate has Infinity as values. Replacing both with 4.5
MLdf[(MLdf$CRR == Inf) , 'CRR'] = 4.5
MLdf[(MLdf$partnershipRR == Inf) , 'partnershipRR'] = 4.5


#Creating basic linear regression model
CricketScoreModel = lm(runScored ~ ., data = MLdf)


x <- summary(CricketScoreModel)$coefficients

x <- cbind(MLdf, CricketScoreModel$fitted.values)








ggplot(data = noBallsByYear, aes(x = matchYear, y = noBallsPerMatch, color = BowlingTeam)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_vline(xintercept =   2008, linetype = 'dashed', color = 'brown') +
  labs(
    x = 'Year',
    y = 'Noballs per Match',
    title = 'Impact of Free Hit Regulation on Noballs Bowled by Teams',
    subtitle = 'Exploring Changes from 2003 to 2023',
    caption = 'Data Source: cricsheet.org'
  ) +
  scale_x_continuous(breaks = c(seq(2003, 2023, 2), 2008)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = 'right'
  )








