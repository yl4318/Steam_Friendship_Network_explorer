

shinyUI(navbarPage(theme = shinytheme("cyborg"), 
                   title = "Steam Explorer", 
                   collapsible = TRUE,
                   
                   tabPanel("Friendship",
                              fluidRow(
                                column(3),
                                column(6,includeMarkdown("src/friendship1.md")),
                                column(3)
                              ),
                              fluidRow(
                                column(3),
                                column(6,
                                       textInput("ID",label="Enter a Steam ID to continue",value="76561198802503180",width="600px"),
                                       actionButton("submit",label="Submit"),
                                       textOutput("test"),
                                       style = "font-size:30px;"
                                ),
                                column(3)
                              ),
                              fluidRow(
                                column(3,htmlOutput("p1"),style = "text-align:right;font-size: 20px;"),
                                column(6,forceNetworkOutput('friendship_network', width = "100%", height = "700px")),
                                column(3,htmlOutput("p2"),style = "text-align:left;")
                              ),
                            fluidRow(
                              column(3),
                              column(6,textOutput("t1"),style = "text-align:center;"),
                              column(3)
                            ),
                            fluidRow(
                              htmlOutput("t2")
                            ),
                              fluidRow(
                                column(3),
                                column(6,plotlyOutput("top10_own")),
                                column(3)
                              ),
                              fluidRow(
                                column(3),
                                column(6,plotlyOutput("top10_meantime")),
                                column(3)
                              ),
                              fluidRow(
                                column(3),
                                column(6,plotlyOutput("top10_totaltime")),
                                column(3)
                              )
                            )
                   
                  
    )
)
