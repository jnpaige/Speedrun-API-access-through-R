require(httr)
require(jsonlite)
require(dplyr)
require(here)
require(lubridate)
require(ggplot2)


# get game id based on its speedrun.com abbreviation

get.game.id<-function(game.string, user.string){
  
  query <- paste("https://www.speedrun.com/api/v1/games?abbreviation=",game.string, sep="",collapse="")
  
  return(unique(fromJSON(content(GET(url=query,add_headers(Authorization=user.string)),type="text"))$data$id))
  
}

#Searching by leader board excludes runs that haven't been validated and excludes retracted runs
#it also presents runs by mod defined categories, and subcategories, and in descending order of completion time


get.game.leaderboard<-function(game.id, cat.id, user.string){
  
  query <- paste("https://www.speedrun.com/api/v1/leaderboards/",game.id,"/category/",cat.id, sep="",collapse="")
  d  <- fromJSON(content(GET(url=query,add_headers(Authorization=user.string,
                                                   `reason.for.req`=reason)),type="text"))

  return(d$data$runs$run)

}


#get a list of three dataframes, one with hexid/labels for categories, and one with same pair info for subcats.


get.categories.incl.subcat<-function(game.id,game.string,user.string){
  list<-list()
  query <- paste("https://www.speedrun.com/api/v1/games/",game.id,"/categories", sep="",collapse="")
  cats  <- fromJSON(content(GET(url=query,add_headers(Authorization=user.string,
                                                      `reason.for.req`=reason)),type="text"))
  #Get Category id and category name
  categories<-cats$data[, grep("id|name", names(cats$data))]
  
  
  #get all subcategories, and subcategory names
  query <- paste("https://www.speedrun.com/api/v1/games/",game.id,"/variables", sep="",collapse="")
  subcats  <- fromJSON(content(GET(url=query,add_headers(Authorization=user.string,
                                                         `reason.for.req`=reason)),type="text"))
  subcats<-subcats$data$values$values
  
  v<-c()
  for(i in 1:length(subcats)){
    label<-subcats[[i]]$label[-which(is.na(subcats[[i]]$label))]
    v[i]<-label
  }
  
  subcat.df<-as.data.frame(cbind(names(subcats),v))
  names(subcat.df)<-c("id","name")
  list<-list(categories, subcat.df)
  names(list)<-c(paste(game.string,"Categories", collapse=" "),paste(game.string,"Subcategories",collapse=""))
  
  return(list)}


get.categories<-function(game.id,user.string){

  query <- paste("https://www.speedrun.com/api/v1/games/",game.id,"/categories", sep="",collapse="")
  cats  <- fromJSON(content(GET(url=query,add_headers(Authorization=user.string,
                                                      `reason.for.req`=reason)),type="text"))
  #Get Category id and category name
  categories<-cats$data[, grep("id|name", names(cats$data))]
  

  return(categories)}





return.game.category.leaderboard<-function(game.id.vector,user.string){
  list<-list()
  
  for(i in 1:length(game.id.vector)){
    Sys.sleep(.5)
    game.id<-game.id.vector[i]
    user.string<-user.string
    categories<-get.categories(game.id,user.string)
    for(j in 1:length(categories$id)){
      Sys.sleep(.5)
      cat<-categories$id[j]
      
      
      leaderboard<-get.game.leaderboard(game.id,cat,user.string)
      #add the most important time to leaderboard, and clean the leaderboard so its a df.
      leaderboard$realtime_t<-leaderboard$times$realtime_t
      leaderboard<-leaderboard[sapply(leaderboard, class) != "data.frame"]
      leaderboard<-leaderboard[sapply(leaderboard, class) != "list"]
      list[[j]]<-leaderboard
      }
    Sys.sleep(.5)
  }
  return(do.call(plyr::rbind.fill,list))
}

## Working on a function that builds big df with cat ids, cat names, and game ids in one df
create.game.id.cat.df<-function(game.id.vector,user.string){
game.id.cat.list<-list()
Sys.sleep(.7)
for(k in 1:length(game.id.vector)){
  cats<-get.categories(game.id.vector[k],user.string)
  cats$game<-game.id.vector[k]
  game.id.cat.list[[k]]<-cats
  Sys.sleep(.7)
}
  return(do.call(plyr::rbind.fill,game.id.cat.list))
  }

create.id.vector<-function(game.abbv.vector,user.string){
  v<-c()
  for(i in 1:length(game.abbv.vector)){
  v[i]<-get.game.id(game.abbv.vector[i],user.string)
  }
  return(v)
}

