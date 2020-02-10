##Libraries needed
library(shiny) #for creating and using shiny
library(shinydashboard) #for creating and using shiny dashboard
library(ggplot2) #for plotting charts
library(leaflet) #for plotting maps

#read file in
utility <- read.table(file = "litterati challenge-65.csv", sep = ",", header = TRUE)
#omit NAs
utility <- na.omit(utility)
#format the timestamp
utility$litterTimestamp <- as.POSIXct(x = utility$litterTimestamp, format = "%Y-%m-%d %H:%M:%S")
#format username,url,tags to character
utility$username <- as.character(utility$username)
utility$url <- as.character(utility$url)
utility$tags <- as.character(utility$tags)
###new dataframe with only lat and lon and 0s removed
newlonlat <- data.frame(newlon=c(utility$lon),newlat=c(utility$lat)) 
row_sub <- apply(newlonlat, 1, function(row) all(row !=0 )) 
#replacing empty tags with "untagged"
utility$tags[utility$tags == ""] <- "untagged"

#UI
ui <- dashboardPage(
  dashboardHeader(title = "424 Project1"),
  dashboardSidebar(width = 300,
    sidebarMenu(
    menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")),
    menuItem("Community Impact",tabName = "lit",startExpanded = F, textOutput("li")),
    menuItem("Page Info",tabName = "til", startExpanded = F, textOutput("il"), textOutput("il2"),textOutput("il3"), textOutput("il4"))
    
    
    )
  ),
  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Forest Park Area", status = "primary", solidHeader = TRUE, width = 12,leafletOutput("map", height = 200)
      )
    ),
    fluidRow(
      box(width = 3, title = "Top 10 Users", status = "primary", solidHeader = TRUE,collapsible = TRUE, 
          column(12,
                 tableOutput("top10users"),style = "height:360px; overflow-y: scroll;"
                 )
      ),
      #yearlytable
      box(width = 3, title = "Top 10 Tags", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        column( 12,
                 tableOutput("tagtypelist"),style = "height:360px; overflow-y: scroll;"
                )
      ),
      box(width = 6, title = "Tags By Date/Time", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          mainPanel(width = 12, style = "height:360px;",
            tabsetPanel(
              tabPanel("Tag Per Weekday",
                width = 12, tableOutput("weeklist"),style = "height:320px; overflow-y: scroll;"
              ),
              tabPanel("Tag Per Hour", tableOutput("hourlist"),style = "height:325px; overflow-y: scroll;" 
                       ),
              tabPanel("Tag Per Date", tableOutput("yearly_chart"),style = "height:325px; overflow-y: scroll;")
            )
          )
      )
    ),
    fluidRow(
      box(width = 12,
        mainPanel(width = 12, 
          tabsetPanel(
           tabPanel("Yearly Barchart",      
                    ##yearlybarchart
                    box( width = 12,title = "Litter by Date", status = "primary", solidHeader = TRUE, plotOutput("year_bar", height = 360)   
                    )
                   ),
           tabPanel("Weekday Barchart",
                    ##weedaybarchart
                    box( width = 12,title = "Litter by Weekday", status = "primary", solidHeader = TRUE, plotOutput("weekday_bar", height = 360)   
                    )
                   ),
           tabPanel("Hourly Barchart",
                    ##hourlybarhcart
                    box( width = 12,title = "Litter by Hour", status = "primary", solidHeader = TRUE, plotOutput("hour_bar", height = 360)   
                    )
                   ),
           tabPanel("Tags Barchart",
                    ##tagtypebarchart
                    box( width = 12,title = "Top 10 Tag-Types", status = "primary", solidHeader = TRUE, plotOutput("tagtype_bar", height = 360)   
                    )
                   )
          )
        )
      )
    )

       
  )
)

server <- function(input, output) { 
  output$map <- renderLeaflet({
    #map of forest park with plot points
     leaflet() %>%
      addTiles() %>%
      addMarkers(data = newlonlat[row_sub,] , lng = ~newlon ,lat = ~newlat,clusterOptions = markerClusterOptions()) %>%
      setView(lng = -87.80666344 , lat = 41.870496518 , zoom = 15)
    
  })
  output$top10users <- renderTable({
    #top 10 in user list
    tab <- table(a<-c(utility$username))
    userrank <- as.data.frame(tab)
    names(userrank) <- c("username","tags")
    userrank$rank <- rank(-userrank$tags,ties.method="min")
    userrank <- userrank[order(userrank$rank,decreasing = F),]
    newten <- userrank[1:10,]
    newten
  })

  output$year_bar <- renderPlot({
    ###making bar chart o f litter throughout the year
    year <- as.Date(utility$litterTimestamp,'%Y-%m-%d')
    p <- ggplot(utility) + aes(x = year) + geom_bar(fill="steelblue")
    p
  })
  output$yearly_chart <- renderTable({
    ###making yearly list
    year <- as.Date(utility$litterTimestamp,'%Y-%m-%d')
    year <- as.character(year)
    yt <- table(a<-c(year))
    yeartab <- as.data.frame(yt)
    names(yeartab) <- c("year","amount")
    yeartab$rank <- rank(-yeartab$amount,ties.method="min")
    yeartab <- yeartab[order(yeartab$rank,decreasing = F),]
    yeartab
  })
  output$weeklist <- renderTable({
    #tags per weekday
    z  <- weekdays(utility$litterTimestamp)
    w <- as.character(z)
    
    we <- table(a<-c(w))
    weekdaytab <- as.data.frame(we)
    names(weekdaytab) <- c("weekday","tags")
    weekdaytab$rank <- rank(-weekdaytab$tags,ties.method="min")
    weekdaytab <- weekdaytab[order(weekdaytab$rank,decreasing = F),]
    weekdaytab
  })
  output$hourlist <- renderTable({
    ##table for hour plot
    nh = format(as.POSIXct(utility$litterTimestamp,format="%H:%M:%S"),"%H")
    h <- as.character(nh)
    
    newh <- table(a<-c(h))
    hourtab <- as.data.frame(newh)
    names(hourtab) <- c("hour","totals")
    hourtab$rank <- rank(-hourtab$totals,ties.method="min")
    hourtab <- hourtab[order(hourtab$rank,decreasing = F),]
    hourtab
  })
  output$tagtypelist <- renderTable({
    #top tag types
    tabb <- table(a<-c(utility$tags))
    ntags <- as.data.frame(tabb)
    names(ntags) <- c("tag","amount")
    #userrank <- head(userrank[order(-userrank$Freq),], n=10)
    ntags$rank <- rank(-ntags$amount,ties.method="min")
    ntags <- ntags[order(ntags$rank,decreasing = F),]
    
    ntags$tag[(grepl(',', ntags$tag)) == TRUE] <- NA
    ntags <- na.omit(ntags)
    ntags[1:10,]
  })
  output$weekday_bar <- renderPlot({
    #weekday barchartplot
    weekday <- weekdays(as.Date(utility$litterTimestamp,'%Y-%m-%d'))
    p <- ggplot(utility) + aes(x = weekday) + geom_bar(color="black",fill="steelblue")
    p
  })
  output$hour_bar <- renderPlot({
    #hourly barchartplot
    hour = format(as.POSIXct(utility$litterTimestamp,format="%H:%M:%S"),"%H")
    q <- ggplot(utility) + aes(x = hour) + geom_bar(color="black",fill="steelblue")
    q
  })
  output$tagtype_bar <- renderPlot({
    #tagtype barchartplot
    tabbb <- table(a<-c(utility$tags))
    ntagss <- as.data.frame(tabbb)
    names(ntagss) <- c("tag","amount")

    ntagss$rank <- rank(-ntagss$amount,ties.method="min")
    ntagss <- ntagss[order(ntagss$rank,decreasing = F),]
    
    ntagss$tag[(grepl(',', ntagss$tag)) == TRUE] <- NA
    ntagss <- na.omit(ntagss)
    
    z <- ggplot(ntagss[1:10,]) + aes(x=tag, y = amount) + geom_bar(stat = "identity",color="black",fill="steelblue")
    z
  })
  output$li <- renderText({
    #total litter text
    text2 <- as.character("Total Items of Litter Picked Up: 12646")
    text2
  })
  output$il <- renderText({
    ##about this project
    text1 <- as.character("Coded By: Ivan Madrid")
                         # Libraries used: shiny,shinydashboard,leaflet,ggplot2
                          #Data Source: https://www.evl.uic.edu/aej/424/litterati challenge-65.csv\n")
    text1
  })
  output$il2 <- renderText({
    ##about this project
    text3 <- as.character("Libraries: shiny,shinydashboard,leaflet,ggplot2")
    text3
  })
  output$il3 <- renderText({
    ##about this project
    text3 <- as.character("Data_Source:")
    text3
  })
  output$il4 <- renderText({
    ##about this project
    text3 <- as.character("www.evl.uic.edu/aej/424/litterati challenge-65.csv")
    text3
  })
}


shinyApp(ui, server)