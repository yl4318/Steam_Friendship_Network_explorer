library(httr)
library(jsonlite)

url<-"http://api.steampowered.com/"
key="A795AF49E5774933C5B36BEE26064485"
# #All APIs and their functions that can be queried 
# api<-GET("http://api.steampowered.com/ISteamWebAPIUtil/GetSupportedAPIList/v0001/?key=A795AF49E5774933C5B36BEE26064485")
# ct_api<-data.frame(fromJSON(content(api, "text")))
# #All apps
# apps<-GET("https://api.steampowered.com/ISteamApps/GetAppList/v2/?key=A795AF49E5774933C5B36BEE26064485")
# ct_app<-data.frame(fromJSON(content(apps, "text")))

#get friends of ids
get_friends_network<-function(id){
  nodes=data.frame(ID=id,group=1)
  #endpoint for friends
  url_<-paste(url,"ISteamUser/GetFriendList/v0001/?",sep='')
  #Get depth 1 friends 
  f<-GET(url_,query=list(key=key,steamid=id,relationship="friend"))
  if(status_code(f)!=200)return (paste(id,status_code(f)))
  
  friends1<-data.frame(fromJSON(httr::content(f,"text")))
  if(nrow(friends1>0)){
    nodes<-rbind(nodes,data.frame(ID=friends1$friendslist.friends.steamid,group=2))
    link<-data.frame(Source=id,Target=friends1$friendslist.friends.steamid)
  }else return("No Friends Found")
  
  
  if(nrow(nodes)>=100){
    for (k in nodes[2:100,1]){
      f<-GET(url_,query=list(key=key,steamid=k,relationship="friend"))
      if(status_code(f)==401|status_code(f)==403)next
      if(status_code(f)!=200)return (paste(id,status_code(f)))
      friends2<-data.frame(fromJSON(httr::content(f,"text")))
      for(l in friends2$friendslist.friends.steamid)
        if(l %in% nodes[2:100,1])link<-rbind(link,data.frame(Source=k,Target=l))
    }
    link<-link[link$Target%in%nodes[1:100,1]&link$Source%in%nodes[1:100,1],]
    return(list(link,nodes[1:100,]))
  }

  #Get depth 2 friends 
  
  for (k in friends1$friendslist.friends.steamid){
    f<-GET(url_,query=list(key=key,steamid=k,relationship="friend"))
    if(status_code(f)==401|status_code(f)==403)next
    if(status_code(f)!=200)return (paste(id,status_code(f)))
    friends2<-data.frame(fromJSON(httr::content(f,"text")))
    link<-rbind(link,data.frame(Source=k,Target=friends2$friendslist.friends.steamid))
    mask<-!friends2$friendslist.friends.steamid%in%nodes$ID
    if(sum(mask) == 0) next # added by Chengwei
    nodes<-rbind(nodes,data.frame(ID=friends2$friendslist.friends.steamid[mask],group=3))
    if(nrow(nodes)>=100)break
  }
  if(nrow(nodes)>=100){
    for (k in nodes[1+nrow(friends1)+1:100,1]){
      f<-GET(url_,query=list(key=key,steamid=k,relationship="friend"))
      if(status_code(f)==401|status_code(f)==403)next
      if(status_code(f)!=200)return (paste(id,status_code(f)))
      friends3<-data.frame(fromJSON(httr::content(f,"text")))
      for(l in friends3$friendslist.friends.steamid)
        if(l %in% nodes[1+nrow(friends1)+1:100,1])link<-rbind(link,data.frame(Source=k,Target=l))
    }
    link<-link[link$Target%in%nodes[1:100,1]&link$Source%in%nodes[1:100,1],]
    return(list(link,nodes[1:100,]))
  }
  return(list(link,nodes))
}

#Get names for friends
get_player_names<-function(nodes){
  url_<-paste(url,"ISteamUser/GetPlayerSummaries/v0002/?",sep='')
  players<-paste(nodes[is.na(nodes)==F],collapse = ",")
  f<-GET(url_,query=list(key=key,steamids=players))
  if(status_code(f)!=200)return (status_code(f))
  return(data.frame(fromJSON(httr::content(f,"text")))[,c(1,4)])
}

#Get owned games for yourself, depth 1 friends and others that have public profile settings
#with playtime 
#possible top-5 charts by sum of time, mean of time, ownership counts.
get_games<-function(nodes){
  url_<-paste(url,"IPlayerService/GetOwnedGames/v0001/?",sep='')
  player_games<-data.frame()
  nodes<-nodes[is.na(nodes)==F]
  cnames <- c('response.game_count', 'response.games.appid',
              'response.games.name', 'response.games.playtime_forever')
  # @param "cname" added by Chengwei
  for(id in nodes){
    f<-GET(url_,query=list(key=key,steamid=id,format="json",
                           include_appinfo="true",	include_played_free_games="true"))
    if(status_code(f)!=200)return (paste(id,status_code(f)))
    games<-data.frame(fromJSON(httr::content(f,"text")))
    # if(nrow(games)>0)player_games<-rbind(player_games,data.frame(ID=id,games[,1:4]))
    if(sum(cnames %in% colnames(games)) != 4) next
    if(nrow(games)>0)player_games<-rbind(player_games,
                                         data.frame(ID=id,games[,cnames]))
    # ^^^ altered by Chengwei, some of the data frame "games" returned is not the same 
    # order of colnames. see example of "76561198068357000"
  }
  return(player_games)
}
