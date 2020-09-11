library(shiny)
library(dplyr)
library(ggplot2)
library(ggdark)
library(reactable)
library(GAlogger)
library(ggrepel)
library(glue)
library(shinyWidgets)
library(plyr)
source("HelpersFBREF.R")
library(magrittr)
library(shinythemes)
ga_set_tracking_id("UA-175572271-1")
ga_set_approval(consent = TRUE)


ELCL <- readRDS("ALL.rds")

AllSquad <- readRDS("AllSquad.rds")


AllSquad[2:172] %<>% mutate_if(is.character,as.numeric)
AllSquad[is.na(AllSquad)] <- 0
ChoicesList <- colnames(ELCL)[c(3:109,111:136)]
ChoicesListSquad <- colnames(AllSquad)[c(2:172)]
ChoicesListSquad <- sort(ChoicesListSquad)
ChoicesList <- sort(ChoicesList)
ui <- navbarPage(theme = shinytheme("yeti"),selected = "Players",
  tags$head(HTML(
  
  "<script>
      (function(i,s,o,g,r,a,m){
        i['GoogleAnalyticsObject']=r;i[r]=i[r]||
        function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
          a=s.createElement(o), m=s.getElementsByTagName(o)[0];
          a.async=1;
          a.src=g;m.parentNode.insertBefore(a,m)
        })
      (window, document, 'script',
        '//www.google-analytics.com/analytics.js','ga');
      
        ga('create', 'UA-175572271-1', 'auto');
        ga('send', 'pageview');
      
      </script>"
)),

# Application title
title = "Create your own FBref scatter plot - A Shiny app by @RobinWilhelmus",


  tabPanel("Players",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(6, 
                               radioButtons("season", "Season:",
                                            c("19/20" = "2019",
                                              "20/21" = "2020"),
                                            width = "50"),
                               checkboxGroupInput(inputId = "Competition",
                                                  label = "Select Competition(s):",
                                                  choices = unique(ELCL$comp),
                                                  selected =  unique(ELCL$comp))),
                        column(3,
                               radioButtons("typeX", "X Axis:",
                                            c("Sum" = "normX",
                                              "Per 90" = "p90X"),
                                            width = "50"),
                               radioButtons("sum", "Sum values across competitions:",
                                            c("Yes (Slow)" = "yes",
                                              "No" = "no"),
                                            selected = "no",
                                            width = "50")),
                        column(3,
                               radioButtons("typeY", "Y Axis:",
                                            c("Sum" = "normY",
                                              "Per 90" = "p90Y"),
                                            width = "50")),
                        
                        
                        
                        hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                        
                        pickerInput(
                          inputId = "teams", label = "Teams to show players from:",
                          choices = unique(ELCL$Squad),
                          options = list(`actions-box` = TRUE,`live-search`=TRUE), 
                          selected = ELCL$Squad,
                          multiple = TRUE
                        ),    
                        selectInput('x', 'X', 
                                    
                                    selected = ChoicesList[sample(3:133,1)],
                                    choices = ChoicesList, multiple=FALSE, selectize=TRUE),
                        
                        
                        selectInput('y', 'Y', 
                                    selected = ChoicesList[sample(3:133,1)],
                                    choices = ChoicesList, multiple=FALSE, selectize=TRUE),
                        
                        
                        
                        sliderInput("age", "Age range:",
                                    min = min(ELCL$Age), max(ELCL$Age), value = c(min(ELCL$Age),max(ELCL$Age))
                        ),
                        
                        
                        sliderInput("minNinety", "Minimum number of 90s:",
                                    min = 1, max = 5, value = 3
                        ),
                        numericInput("percX", "See label above certain percentile X:", 99.9, min = 50, max = 100),
                        numericInput("percY", "See label above certain percentile Y:", 99.9, min = 50, max = 100)
                        
               )),
             
             # Show a plot of the generated distribution
             mainPanel("Last update: 29-08-2020",
               tabsetPanel(type = "tabs",
                           tabPanel("Plot", 
                                 #   uiOutput("myPlot"),
                                 
                                    h4("", align = "center"),
                                    
                                    plotOutput("plot2")),
                           # tableOutput("myTable")),
                           tabPanel("Dark Mode", 
                                    
                                    plotOutput("plot3")),
                           tabPanel("Table", 
                                    
                                  )
               )
             )
           )),
  tabPanel("Teams",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(6, 
                               checkboxGroupInput(inputId = "CompetitionSquad",
                                                  label = "Select Competition(s):",
                                                  choices = unique(AllSquad$comp),
                                                  selected =  unique(AllSquad$comp)[c(1:5)])),
                        column(3,
                               radioButtons("typeXSquad", "X Axis:",
                                            c("Sum" = "normX",
                                              "Per Match" = "p90XSquad"),
                                            width = "50")),
                        column(3,
                               radioButtons("typeYSquad", "Y Axis:",
                                            c("Sum" = "normY",
                                              "Per Match" = "p90YSquad"),
                                            width = "50")),
                        
                        
                        
                        hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                        
                        pickerInput(
                          inputId = "teams", label = "Teams to show:",
                          choices = unique(AllSquad$Squad),
                          options = list(`actions-box` = TRUE,`live-search`=TRUE), 
                          selected = ELCL$Squad,
                          multiple = TRUE
                        ),    
                        selectInput('xx', 'X', 
                                    
                                    selected = ChoicesListSquad[sample(10:172,1)],
                                    choices = ChoicesListSquad, multiple=FALSE, selectize=TRUE),
                        
                        
                        selectInput('yy', 'Y', 
                                    selected = ChoicesListSquad[sample(10:172,1)],
                                    choices = ChoicesListSquad, multiple=FALSE, selectize=TRUE),
                        
                        
                        
                        
                        
                        
                        
                        numericInput("percXSquad", "See label above certain percentile X:", 95, min = 50, max = 100),
                        numericInput("percYSquad", "See label above certain percentile Y:", 95, min = 50, max = 100)
                        
               )),
             mainPanel("Last update: 29-08-2020",
               tabsetPanel(type = "tabs",
                                    tabPanel("Plot", 
                                          #   textOutput("selected_var"),
                                             h4("", align = "center"),
                                             plotOutput("plot4")),
                                    # tableOutput("myTable")),
                                    tabPanel("Dark Mode", 
                                             
                                             h4("", align = "center"),
                                             plotOutput("plot5")),
                                    tabPanel("Table", 
                                             
                                             reactableOutput("codes", width = "auto", height = "auto",
                                                             inline = FALSE),
                                             h4("", align = "center"))
             )))
  )
) #Close outer tabsetPanel



server <- function(input, output) {
  
  observeEvent(input$Competition, {
    print(input$Competition)
    sub<-input$Competition
    print(sub)
  })
  myData <- reactive({
    req(input$x) 
    req(input$y)
    req(input$minNinety)
    req(input$Competition)
    req(input$age[1])
    req(input$age[2])
    
    
    if(input$sum == "yes"){
      test <- paste(input$Competition, collapse=" - ")
      filter(ELCL,`90s` >= input$minNinety) %>% filter(comp %in% input$Competition) %>%
        filter(Season %in% input$season) %>%
        filter(Squad %in% input$teams) %>%
        filter(Age>input$age[1] & Age < input$age[2])%>%
        ddply(c("Player","Age","Born"), numcolwise(sum)) %>%
        select(Player,`90s`,input$x,input$y) %>%
        set_colnames(c("Player", "90s", "X","Y"))%>%
        mutate(comp = test) %>%
        
        mutate(xAxis = input$x) %>%
        mutate(yAxis = input$y) 
      
    }else{
      filter(ELCL,`90s` >= input$minNinety) %>% filter(comp %in% input$Competition) %>%
        filter(Squad %in% input$teams) %>%
        filter(Season %in% input$season) %>%
        filter(Age>input$age[1] & Age < input$age[2])%>%
        select(Player,`90s`,input$x,input$y,comp) %>%
        # mutate(subtitle = sub) %>%
        set_colnames(c("Player", "90s", "X","Y","comp"))%>%
        mutate(xAxis = input$x) %>%
        mutate(yAxis = input$y) 
    }
  })
  output$selected_var <- renderText({ 
    input$xx
  })
  myData2 <- reactive({
    req(input$xx) 
    req(input$yy)
    
    req(input$CompetitionSquad)
    
      test <- paste(input$CompetitionSquad, collapse=" - ")
      filter(AllSquad,comp %in% input$CompetitionSquad) %>%
        filter(Squad %in% input$teams) %>%
        
        
        select(Squad,`Matches Played`,input$xx,input$yy,Season) %>%
        set_colnames(c("Squad", "Matches Played", "X","Y","Season"))%>%
        mutate(comp = test) %>%
        
        mutate(xAxis = input$xx) %>%
        mutate(yAxis = input$yy) 
       
    
    
  })
  output$codes <- renderReactable({
    reactable(
      myData(),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      )
    )
  })
  output$codes2 <- renderReactable({
    reactable(
      myData2(),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      )
    )
  })
  output$myTable <- renderTable(myData())
  output$plot2<-renderPlot({
    if(input$typeX == "p90X" & input$typeY != "p90Y"){
      ggplot(myData(),aes(x=as.integer(X)/`90s`,y=as.integer(Y)))+geom_point(colour='#026937') +
        geom_label_repel(data=myData()%>% filter(X/`90s` > quantile(X/`90s`, input$percX/100)|
                                                   Y > quantile(Y, input$percY/100)),aes(label = Player),fill="white",color="black")+
        labs(x=glue::glue("{myData()$xAxis} P90"),
             y=glue::glue("{myData()$yAxis}"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeX == "p90X" & input$typeY == "p90Y") {
      ggplot(myData(),aes(x=as.integer(X)/`90s`,y=as.integer(Y)/`90s`))+geom_point(colour='#026937') +
        geom_label_repel(data=myData()%>% filter(X/`90s` > quantile(X/`90s`, input$percX/100)|
                                                   Y/`90s` > quantile(Y/`90s`, input$percY/100)),aes(label = Player),fill="white",color="black")+
        labs(x=glue::glue("{myData()$xAxis} P90"),
             y=glue::glue("{myData()$yAxis} P90"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeX != "p90X" & input$typeY == "p90Y") {
      ggplot(myData(),aes(x=as.integer(X),y=as.integer(Y)/`90s`))+geom_point(colour='#026937') +
        geom_label_repel(data=myData()%>% filter(X > quantile(X, input$percX/100)|
                                                   Y/`90s` > quantile(Y/`90s`, input$percY/100)),aes(label = Player),fill="white",color="black")+
        labs(x=glue::glue("{myData()$xAxis}"),
             y=glue::glue("{myData()$yAxis} P90"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else{
      
      ggplot(myData(),aes(x=as.integer(X),y=as.integer(Y)))+geom_point(colour='#026937') +
        geom_label_repel(data=myData()%>% filter(X > quantile(X, input$percX/100)|
                                                   Y > quantile(Y, input$percY/100)),aes(label = Player),fill="white",color="black")+
        labs(x=myData()$xAxis,
             y=myData()$yAxis,
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }
  }
  , height = 500, width = 750)
  
  output$plot3<-renderPlot({
    if(input$typeX == "p90X" & input$typeY != "p90Y"){
      ggplot(myData(),aes(x=as.integer(X)/`90s`,y=as.integer(Y)))+geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData()%>% filter(X/`90s` > quantile(X/`90s`, input$percX/100)|
                                                   Y > quantile(Y, input$percY/100)),aes(label = Player),fill="black",color="white")+
        labs(x=glue::glue("{myData()$xAxis} P90"),
             y=glue::glue("{myData()$yAxis}"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeX == "p90X" & input$typeY == "p90Y") {
      ggplot(myData(),aes(x=as.integer(X)/`90s`,y=as.integer(Y)/`90s`))+geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData()%>% filter(X/`90s` > quantile(X/`90s`, input$percX/100)|
                                                   Y/`90s` > quantile(Y/`90s`, input$percY/100)),aes(label = Player),fill="black",color="white")+
        labs(x=glue::glue("{myData()$xAxis} P90"),
             y=glue::glue("{myData()$yAxis} P90"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeX != "p90X" & input$typeY == "p90Y") {
      ggplot(myData(),aes(x=as.integer(X),y=as.integer(Y)/`90s`))+ geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData()%>% filter(X > quantile(X, input$percX/100)|
                                                   Y/`90s` > quantile(Y/`90s`, input$percY/100)),aes(label = Player),fill="black",color="white")+
        labs(x=glue::glue("{myData()$xAxis}"),
             y=glue::glue("{myData()$yAxis} P90"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else{
      ggplot(myData(),aes(x=as.integer(X),y=as.integer(Y)))+geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData()%>% filter(X > quantile(X, input$percX/100)|
                                                   Y > quantile(Y, input$percY/100)),aes(label = Player),fill="black",color="white")+
        labs(x=glue::glue("{myData()$xAxis}"),
             y=glue::glue("{myData()$yAxis}"),
             title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }
  }
  , height = 500, width = 750)
  output$plot4<-renderPlot({
    if(input$typeXSquad == "p90XSquad" & input$typeYSquad != "p90YSquad"){
      ggplot(myData2(),aes(x=as.integer(X)/`Matches Played`,y=as.integer(Y)))+
        geom_point(colour='#026937') +
        geom_label_repel(data=myData2()%>% filter(X/`Matches Played` > quantile(X/`Matches Played`, input$percXSquad/100)|
                                                    Y > quantile(Y, input$percYSquad/100)),aes(label = Squad),fill="white",color="black")+
        labs(x=glue::glue("{myData2()$xAxis} P90"),
             y=glue::glue("{myData2()$yAxis}"),
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeXSquad == "p90XSquad" & input$typeYSquad == "p90YSquad") {
      ggplot(myData2(),aes(x=as.integer(X)/`Matches Played`,y=as.integer(Y)/`Matches Played`))+
        geom_point(colour='#026937') +
        geom_label_repel(data=myData2()%>% filter(X/`Matches Played` > quantile(X/`Matches Played`, input$percXSquad/100)|
                                                    Y/`Matches Played` > quantile(Y/`Matches Played`, input$percYSquad/100)),aes(label = Squad),fill="white",color="black")+
        labs(x=glue::glue("{myData2()$xAxis} P90"),
             y=glue::glue("{myData2()$yAxis} P90"),
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeXSquad != "p90XSquad" & input$typeYSquad == "p90YSquad") {
      ggplot(myData2(),aes(x=as.integer(X),y=as.integer(Y)/`Matches Played`))+
        geom_point(colour='#026937') +
        geom_label_repel(data=myData2()%>% filter(X > quantile(X, input$percXSquad/100)|
                                                    Y/`Matches Played` > quantile(Y/`Matches Played`, input$percYSquad/100)),aes(label = Squad),fill="white",color="black")+
        labs(x=glue::glue("{myData2()$xAxis}"),
             y=glue::glue("{myData2()$yAxis} P90"),
             title = paste0(myData2()$xAxis," and " ,myData()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else{
      
      ggplot(myData2(),aes(x=as.integer(X),y=as.integer(Y)))+geom_point(colour='#026937') +
        geom_label_repel(data=myData2()%>% filter(X > quantile(X, input$percXSquad/100)|
                                                    Y > quantile(Y, input$percYSquad/100)),aes(label = Squad),fill="white",color="black")+
        labs(x=myData2()$xAxis,
             y=myData2()$yAxis,
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }
  }
  , height = 500, width = 750)
  output$plot5<-renderPlot({
    if(input$typeXSquad == "p90XSquad" & input$typeYSquad != "p90YSquad"){
      ggplot(myData2(),aes(x=as.integer(X)/`Matches Played`,y=as.integer(Y)))+
        geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData2()%>% filter(X/`Matches Played` > quantile(X/`Matches Played`, input$percXSquad/100)|
                                                    Y > quantile(Y, input$percYSquad/100)),aes(label = Squad),fill="black",color="white")+
        labs(x=glue::glue("{myData2()$xAxis} P90"),
             y=glue::glue("{myData2()$yAxis}"),
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeXSquad == "p90XSquad" & input$typeYSquad == "p90YSquad") {
      ggplot(myData2(),aes(x=as.integer(X)/`Matches Played`,y=as.integer(Y)/`Matches Played`))+
        geom_point(colour='#026937') +
        geom_label_repel(data=myData2()%>% filter(X/`Matches Played` > quantile(X/`Matches Played`, input$percXSquad/100)|
                                                    Y/`Matches Played` > quantile(Y/`Matches Played`, input$percYSquad/100)),aes(label = Squad),fill="black",color="white")+
        labs(x=glue::glue("{myData2()$xAxis} P90"),
             y=glue::glue("{myData2()$yAxis} P90"),
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        geom_point(colour='red',alpha=0.5) +
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else  if(input$typeXSquad != "p90XSquad" & input$typeYSquad == "p90YSquad") {
      ggplot(myData2(),aes(x=as.integer(X),y=as.integer(Y)/`Matches Played`))+
        geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData2()%>% filter(X > quantile(X, input$percXSquad/100)|
                                                    Y/`Matches Played` > quantile(Y/`Matches Played`, input$percYSquad/100)),aes(label = Squad),fill="black",color="white")+
        labs(x=glue::glue("{myData2()$xAxis}"),
             y=glue::glue("{myData2()$yAxis} P90"),
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }else{
      
      ggplot(myData2(),aes(x=as.numeric(X),y=as.numeric(Y)))+geom_point(colour='red',alpha=0.5) +
        geom_label_repel(data=myData2()%>% filter(X > quantile(as.numeric(X), input$percXSquad/100)|
                                                    Y > quantile(as.numeric(Y), input$percYSquad/100)),aes(label = Squad),fill="black",color="white")+
        labs(x=myData2()$xAxis,
             y=myData2()$yAxis,
             title = paste0(myData2()$xAxis," and " ,myData2()$yAxis, " 19/20"),
             subtitle= paste(unique(myData2()$comp),collapse=" - "),
             caption = "Data from FBref.com\nMade on ShinyNew.RobinKoetsier.nl/FBrefApp") +
        dark_theme_gray()+
        theme(plot.title = element_text(hjust=0.5, size = 15),
              plot.subtitle = element_text(hjust=0.5))
    }
  }
  , height = 500, width = 750)
 
}

shinyApp(ui = ui, server = server)




