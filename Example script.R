require(httr)
require(jsonlite)
require(dplyr)
require(here)
require(lubridate)
require(ggplot2)

setwd(here::here())

#Supply info to be polite

options(HTTPUserAgent="Jon Paige jon.paige@notmyemail.com Rstudio (for scientific research)")
reason<-" I am studying the evolution of speedrunning strategies. Please email me if my API calls are causing issues. jon.paige@notmyemail.com"

###
###Note: don't share your user API key. These are assigned when a user acquires a speedrun.com account.
###Also: if you are going to be doing research using this tool, make sure that you go through the IRB process
###While these data are "publicly available" these data do have potentially individually identifiable information. 

user.string<-"xxxxxxxxxxxxxxxxxx"

#Collect the abbreviations for each game we are interested in into a vector
game.abbv.vector<-c("the_witcher_3_wild_hunt",
             "mk1",
             "smb3",
             "supermetroid",
             "celeste",
             "sm64",
             "doom_unity",
             "zelda_ii_the_adventure_of_link",
             "nba_jam",
             "mtpo",
             "mortal_kombat_x",
             "tetrisnes",
             "smk",
             "mk8dx")

#Call in functions. 
source("functions.R")


#Get a list of hex IDs assigned to each game
game.id.vector<-create.id.vector(game.abbv.vector, user.string)
game.id.vector

#Produce a dataframe of category hex ids, the name of that run category, and the game hex id
game.categories<-create.game.id.cat.df(game.id.vector, user.string)
game.categories

#Produce a dataframe of all accepted runs, per category. For now, subcategories are excluded
game.runs<-return.game.category.leaderboard(game.id.vector,user.string) 
write.csv(game.runs,"speedruns.csv")
df<-game.runs

#Parse the date with lubridate
df$date.parsed<-parse_date_time(df$date, orders = c('ymd'))

#simple, ugly, example plot
ggplot(data=df, aes(x=date.parsed,y=realtime_t, color=category)) +
  geom_point() +
  geom_smooth() +facet_grid(~game)



