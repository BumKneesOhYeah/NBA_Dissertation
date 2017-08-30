##used to get better list of each column number and name
printnames = function(x)
{
  many =ncol(x)
  for (i in 1:many)
  {
    cat(i,"-",names(x)[i],"\n")
  }
}

data_set_folder = choose.dir(default = "", caption = "Select folder")
setwd(data_set_folder)

### data must look like from:     http://stats.nba.com/teams/boxscores/ 
##
### ************** the firsst column must stated weather PRE, REG, PLAY (parts of season)
## otherwise exact copy of website
# 1 - Season_type 
# 2 - TEAM 
# 3 - MATCH.UP 
# 4 - GAME.DATE 
# 5 - W.L 
# 6 - MIN 
# 7 - PTS 
# 8 - FGM 
# 9 - FGA 
# 10 - X_FG_perc 
# 11 - X_3PM 
# 12 - X_3PA 
# 13 - X3P_Perc 
# 14 - FTM 
# 15 - FTA 
# 16 - FT_perc 
# 17 - OREB 
# 18 - DREB 
# 19 - REB 
# 20 - AST 
# 21 - STL 
# 22 - BLK 
# 23 - TOV 
# 24 - PF 
# 25 - X.....Plus.Minus 

dir.create("averaged_data_folder")


all_files = dir()

# ##what n's do you want?
# n_set = 6:7
# 
# ##what files do you want to use?
# file_set = 1:3





get_n_averages <- function(n_set, file_set){
for (n in 1:length(n_set)){
  setwd(data_set_folder) 
  
  for (file in 1:length(file_set)){

nba = read.csv(all_files[file_set[file]], stringsAsFactors = FALSE)

## erase unused columns
nba = nba[,-c(6,10,13,16,19,25)]

#reorder to be visually easier to read in View(nba)
nba = nba[,c(1,4,3,2,7:19,6,5)]

##Only using regular season and Playoff games (no preseason or games not full played)
nba = nba[nba$season %in% c("REG","PLAY"),]

## format GAME.DATE as date
as.Date(nba$GAME.DATE,format ="%m/%d/%Y") -> nba$GAME.DATE

## seperates two teams and states which is the home team
names(nba)[4] = "team"  # change name
nba$opponent = ""
nba$home_or_away = ""
ifelse(substr(nba$MATCH.UP,5,7) == "vs.",substr(nba$MATCH.UP,9,11),substr(nba$MATCH.UP,7,9)) -> nba$opponent
ifelse(substr(nba$MATCH.UP,5,7) == "vs.","Home","Away") -> nba$home_or_away

## reorder to for visual ease again
nba <- nba[,c(1,2,21,4,20,19,5:18)]

## gets the game number and date from beginning to end of season
## then stores the game number for each game so a join/merge can be done to put
##    home team and away team for each game in same row
days_in_season = as.numeric(max(nba$GAME.DATE) - min(nba$GAME.DATE)) + 1
x = as.data.frame(c(1:days_in_season))
x$date = as.Date(min(nba$GAME.DATE),format="%Y-%m-%d")
#View(x)
for(i in 2:days_in_season)
  {x$date[i]=x$date[i-1]+1}
nba$day_in_season = 0
number_of_games = nrow(nba)
for (i in 1:number_of_games)
{
  for(j in 1:days_in_season)
  {
    if (nba$GAME.DATE[i]==x$date[j])
    {
      nba$day_in_season[i] = x$`c(1:days_in_season)`[j]
      break
    }
  }
}
nba$team_game_in_season = 0
names(nba)[1]="Season_type"
#gets ll the tunique teams for season
teams = as.data.frame(unique(nba[nba$Season_type=="REG",4]))
names(teams)[1]="team"
teams$count=1
teams$team = as.character(teams$team)
nba = nba[order(nba$GAME.DATE),]
for(i in 1:number_of_games)
{
  for (j in 1:nrow(teams))
  {
    if (teams$team[j] == nba$team[i])
    {
      nba$team_game_in_season[i] = teams$count[j]
      teams$count[j] = teams$count[j] + 1
      break
    }
  }
}

teams$count = teams$count - 1

nba = (nba[order(nba$team,nba$team_game_in_season),])
nba = nba[,c(1,2,21,22,3:20)]

nba$season_year = substr(all_files[file_set[file]],13,19)

nba = nba[,c(23,1:22)]


##aggregate the averages start
aggregate_final = nba[1,]
teams = unique(nba$team)


for (i in 1:length(teams))
{
  thing = nba[nba$team==teams[i],]
  length_of_thing = nrow(thing)

  for (j in 1:length_of_thing)
  {
    if(j==1)
    {
      season_year = thing[j,1]
      Season_type = thing[j,2]
      GAME.DATE = thing[j,3]
      day_in_season = thing[j,4]
      team_game_in_season = thing[j,5]
      home_or_away= thing[j,6]
      team = thing[j,7]
      opponent = thing[j,8]

      W.L= thing[j,9]

      FGM = NA
      FGA = NA
      X_3PM = NA
      X_3PA = NA
      FTM = NA
      FTA = NA
      OREB = NA
      DREB = NA
      AST = NA
      STL = NA
      BLK = NA
      TOV = NA
      PF = NA
      PTS = NA

     

      aggregate_final = rbind(aggregate_final,data.frame(season_year, Season_type, GAME.DATE,
                                                         day_in_season, team_game_in_season,
                                                         home_or_away, team, opponent, W.L, FGM, FGA, X_3PM,
                                                         X_3PA, FTM, FTA, OREB, DREB, AST, STL, BLK,
                                                         TOV, PF, PTS))
    } else if (j < n_set[n]+1)
    {
      season_year = thing[j,1]
      Season_type = thing[j,2]
      GAME.DATE = thing[j,3]
      day_in_season = thing[j,4]
      team_game_in_season = thing[j,5]
      home_or_away= thing[j,6]
      team = thing[j,7]
      opponent = thing[j,8]
      
      W.L= thing[j,9]

      FGM = mean(thing[c(1:j-1),10])
      FGA = mean(thing[c(1:j-1),11])
      X_3PM = mean(thing[c(1:j-1),12])
      X_3PA = mean(thing[c(1:j-1),13])
      FTM = mean(thing[c(1:j-1),14])
      FTA = mean(thing[c(1:j-1),15])
      OREB = mean(thing[c(1:j-1),16])
      DREB = mean(thing[c(1:j-1),17])
      AST = mean(thing[c(1:j-1),18])
      STL = mean(thing[c(1:j-1),19])
      BLK = mean(thing[c(1:j-1),20])
      TOV = mean(thing[c(1:j-1),21])
      PF = mean(thing[c(1:j-1),22])
      PTS = mean(thing[c(1:j-1),23])


      aggregate_final = rbind(aggregate_final,data.frame(season_year, Season_type, GAME.DATE,
                                                         day_in_season, team_game_in_season,
                                                         home_or_away, team, opponent, W.L, FGM, FGA, X_3PM,
                                                         X_3PA, FTM, FTA, OREB, DREB, AST, STL, BLK,
                                                         TOV, PF, PTS))

    } else
    {
      season_year = thing[j,1]
      Season_type = thing[j,2]
      GAME.DATE = thing[j,3]
      day_in_season = thing[j,4]
      team_game_in_season = thing[j,5]
      home_or_away= thing[j,6]
      team = thing[j,7]
      opponent = thing[j,8]
      
      W.L= thing[j,9]

      interval = c((j-n_set[n]):(j-1))
      FGM =   mean(thing[interval,10])
      FGA =   mean(thing[interval,11])
      X_3PM = mean(thing[interval,12])
      X_3PA = mean(thing[interval,13])
      FTM =   mean(thing[interval,14])
      FTA =   mean(thing[interval,15])
      OREB =  mean(thing[interval,16])
      DREB =  mean(thing[interval,17])
      AST =   mean(thing[interval,18])
      STL =   mean(thing[interval,19])
      BLK =   mean(thing[interval,20])
      TOV =   mean(thing[interval,21])
      PF =    mean(thing[interval,22])
      PTS =   mean(thing[interval,23])

      aggregate_final = rbind(aggregate_final,data.frame(season_year, Season_type, GAME.DATE,
                                                         day_in_season, team_game_in_season,
                                                         home_or_away, team, opponent, W.L, FGM, FGA, X_3PM,
                                                         X_3PA, FTM, FTA, OREB, DREB, AST, STL, BLK,
                                                         TOV, PF, PTS))

    }
  }

}
aggregate_final = aggregate_final[-1,]
##aggregate the averages end

#now seperate home and away team to combine all in one!!
home_final = aggregate_final[aggregate_final$home_or_away=="Home",]
away_final = aggregate_final[aggregate_final$home_or_away=="Away",]


home_final$away_season_year = ""; home_final$away_Season_type = ""; home_final$away_GAME.DATE = home_final$GAME.DATE ;
home_final$away_day_in_season = 0; home_final$away_team_game_in_season = 0;
home_final$away_home_or_away = ""; home_final$away_team = "";
home_final$away_opponent = "";
home_final$away_W.L = ""; home_final$away_FGM = 0; home_final$away_FGA = 0; home_final$away_X_3PM = 0;
home_final$away_X_3PA = 0; home_final$away_FTM = 0; home_final$away_FTA = 0; home_final$away_OREB = 0;
home_final$away_DREB = 0; home_final$away_AST = 0; home_final$away_STL = 0; home_final$away_BLK = 0;
home_final$away_TOV = 0; home_final$away_PF = 0; home_final$away_PTS = 0;




length_of_home = nrow(home_final)
for (i in 1:length_of_home)
{
  temp = away_final[away_final$GAME.DATE==home_final$GAME.DATE[i],]
  zeta = temp[temp$team==home_final$opponent[i],]
  for(j in 1:23)
  {
    home_final[i,j+23] = zeta[1,j]
  }
}

names(home_final)= c("home_season_year","home_Season_type","home_GAME.DATE","home_day_in_season",
                     "home_team_game_in_season","home_home_or_away","home_team",
                     "home_opponent","home_W.L","home_FGM",
                     "home_FGA","home_X_3PM","home_X_3PA","home_FTM","home_FTA","home_OREB",
                     "home_DREB","home_AST","home_STL","home_BLK","home_TOV","home_PF",
                     "home_PTS","away_season_year","away_Season_type",
                     "away_GAME.DATE","away_day_in_season",
                     "away_team_game_in_season",
                     "away_home_or_away","away_team","away_opponent","away_W.L","away_FGM",
                     "away_FGA","away_X_3PM","away_X_3PA",
                     "away_FTM","away_FTA","away_OREB",
                     "away_DREB","away_AST","away_STL",
                     "away_BLK","away_TOV","away_PF",
                     "away_PTS")

names(home_final)[1]=	"season_year"
names(home_final)[2]=	"reg.play"
names(home_final)[3] = "GAME.DATE"
names(home_final)[4]=	"day_in_season"
names(home_final)[9]	="W.L"



model_ready = home_final[,c(1:4,9,7,5,30,28,10:23,33:46)]


if(file == 1)
{
all_season = model_ready
}
else
{
  all_season = rbind(all_season,model_ready)
}

}
  setwd(paste0(getwd(), "/averaged_data_folder"))
  write.csv(all_season,paste("all_seasons_by_n_",n_set[n],".csv",sep=""),row.names = F)
}
}










# write.csv(all_season, "all_season.csv",row.names = FALSE)
