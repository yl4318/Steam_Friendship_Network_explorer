library(shiny)
source("src/api.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    #For friendship network
    observeEvent(input$submit, {
      id<-reactiveValues(id=input$ID)
      cnames <- c('ID', 'response.game_count', 'response.games.appid',
                  'response.games.name', 'response.games.playtime_forever')
      if(str_detect(id$id,"7656[0-9]{13}\\b")){
        a<-get_friends_network(as.character(id$id))
        if(is.list(a)==F)output$test<-renderText({"No friends available or profile set to private."})
        else {output$test<-renderText({paste("Showing results for:",id$id)})
        b<-get_player_names(a[[2]]$ID)
        c<-get_games(a[[2]]$ID)
        # if(sum(cnames %in% colnames(c)) != 5) next
        games<-c%>%group_by(name=response.games.name)%>%
          summarise(n=n(),mean_time=round(mean(response.games.playtime_forever),2),total_time=sum(response.games.playtime_forever))
        nodes<-data.frame(a[[2]],size=1)
        nodes<-left_join(nodes,b,by=c("ID"="response.players.steamid"))
        h<-hash(data.frame(nodes$ID,seq(0,nrow(nodes)-1)))
        links<-data.frame(Source=hash_look(a[[1]]$Source,h),Target=hash_look(a[[1]]$Target,h),value=1)  
        
        output$friendship_network <- renderForceNetwork(
          forceNetwork(Links = links, Nodes = nodes,
                       Source = 'Source', Target = 'Target', Value = "value",
                       NodeID = 'response.players.personaname', Group = 'group', 
                       Nodesize ="size", fontSize = 10,
                       opacityNoHover = 0.7,
                       opacity = 1,
                       zoom=T,
                       clickAction = 'Shiny.onInputChange("id", d.name)', 
                       colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
          )
        )
        output$top10_own<-renderPlotly({
          data<-games%>%arrange(desc(n),name)
          p<-ggplot(data[1:10,])+geom_col(aes(x=name,y=n),fill="green4")+coord_flip()+
            labs(title="Top 10 Most Owned Games",xlab="Count",ylab="Game")+
            ggthemes::theme_fivethirtyeight()
          ggplotly(p=p)
        })
        output$top10_meantime<-renderPlotly({
          data<-games%>%arrange(desc(mean_time),name)
          p<-ggplot(data[1:10,])+geom_col(aes(x=name,y=mean_time),fill="yellow4")+coord_flip()+
            labs(title="Top 10 Most Played Games\nBy Average Playtime",xlab="Average Playtime",ylab="Game")+
            ggthemes::theme_fivethirtyeight()
          ggplotly(p=p)
        })
        output$top10_totaltime<-renderPlotly({
          data<-games%>%arrange(desc(total_time),name)
          p<-ggplot(data[1:10,])+geom_col(aes(x=name,y=mean_time),fill="coral4")+coord_flip()+
            labs(title="Top 10 Most Played Games\nBy Total Playtime",xlab="Total Playtime",ylab="Game")+
            ggthemes::theme_fivethirtyeight()
          ggplotly(p=p)
        })
        output$t1<-renderText({
          "Below are 10 most popular games chosen by different criteria. Note that from above network we are able to access your friends'
      owned games and playtime information only if his profile is set to Public."
        })
        output$p1<-renderText({
          "SCROLL to zoom in and out<br/>CLICK nodes to see their name  <br/>DRAG nodes to change positions  <br/>DRAG blank to move the plot"
        })
        output$p2<-renderText({
          "This plot includes 1st and 2nd layer of your friendship circle starting from you and have maximum 100 people capacity, 
          which means that if you have more than 99 friends, 
          you would not be able to see their friends, though you still can see the relations among these 99 of your friends."
        })
        output$t2<-renderText({
          "<br/>"
        })
        }
      }else {
        output$test<-renderText({
          "Unable to retrieve data, please try another ID."
        })
      }
    })
    
    

})
