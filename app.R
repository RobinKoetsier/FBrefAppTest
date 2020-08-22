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
ga_set_tracking_id("UA-175572271-1")
ga_set_approval(consent = TRUE)

#ELCL <- readRDS("fbrefdata.rds")
ELCL <- readRDS("ALL.rds")
#ELCLSum <- readRDS("ALLSUM.rds")

ChoicesList <- colnames(ELCL)[c(3:136)]
ChoicesList <- sort(ChoicesList)
ui <- fluidPage(tags$head(HTML(
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
    titlePanel("Create your own FBref scatter plot - A Shiny app by @RobinWilhelmus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fluidRow(column(6, 
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
                          
                            selected = "Progressive Passes",
                            choices = ChoicesList, multiple=FALSE, selectize=TRUE),
            
          
                selectInput('y', 'Y', 
                            selected = "Assists",
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
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", 
                                 h4("", align = "center"),
                                 
                                 plotOutput("plot2")),
                               # tableOutput("myTable")),
                        tabPanel("Dark Mode", 
                                 
                                 plotOutput("plot3")),
                        tabPanel("Table", 
                               
                                 reactableOutput("codes", width = "auto", height = "auto",
                                                 inline = FALSE))
            )
        )
    )
)


server <- function(input, output) {
  observeEvent(input$Competition, {
    print(input$Competition)
    sub<-input$Competition
  })
    myData <- reactive({
      req(input$x) 
      req(input$y)
      req(input$minNinety)
      req(input$Competition)
      req(input$age[1])
      req(input$age[2])
      req(sub)
      if(input$sum == "yes"){
      
      filter(ELCL,`90s` >= input$minNinety) %>% filter(comp %in% input$Competition) %>%
        filter(Squad %in% input$teams) %>%
      filter(Age>input$age[1] & Age < input$age[2])%>%
       ddply(c("Player","Age","Born"), numcolwise(sum)) %>%
      select(Player,`90s`,input$x,input$y) %>%
      # mutate(subtitle = sub) %>%
    
      mutate(xAxis = input$x) %>%
      mutate(yAxis = input$y) %>%
      setNames(gsub(input$x, "X", names(.))) %>%
        setNames(gsub(input$y, "Y", names(.))) 
      
      }else{
        filter(ELCL,`90s` >= input$minNinety) %>% filter(comp %in% input$Competition) %>%
          filter(Squad %in% input$teams) %>%
          filter(Age>input$age[1] & Age < input$age[2])%>%
          select(Player,`90s`,input$x,input$y) %>%
          # mutate(subtitle = sub) %>%
          
          mutate(xAxis = input$x) %>%
          mutate(yAxis = input$y) %>%
          setNames(gsub(input$x, "X", names(.))) %>%
          setNames(gsub(input$y, "Y", names(.))) 
      }
        
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
    output$myTable <- renderTable(myData())
    output$plot2<-renderPlot({
        if(input$typeX == "p90X" & input$typeY != "p90Y"){
            scatterMaken90X(df = myData(),
                         percX=input$percX,
                         percY=input$percY)
        }else  if(input$typeX == "p90X" & input$typeY == "p90Y") {
        scatterMaken90(df = myData(),
                     percX=input$percX,
                     percY=input$percY)
        }else  if(input$typeX != "p90X" & input$typeY == "p90Y") {
            scatterMaken90Y(df = myData(),
                           percX=input$percX,
                           percY=input$percY)
        }else{
          uniek<-input$Competition
          ggplot(myData(),aes(x=as.integer(X),y=as.integer(Y)))+geom_point(colour='#026937') +
            geom_label_repel(data=myData()%>% filter(X > quantile(X, input$percX/100)|
                                                 Y > quantile(Y, input$percY/100)),aes(label = Player),fill="white",color="black")+
            labs(x=myData()$xAxis,
                 y=myData()$yAxis,
                 title = paste0(myData()$xAxis," and " ,myData()$yAxis, " 19/20"),
                # subtitle= substitute(uniek),
                 caption = "Data from FBref.com\n@RobinWilhelmus") +
            theme_bw()+
            theme(plot.title = element_text(hjust=0.5, size = 15))
        }
        }
        , height = 500, width = 750)
    output$plot3<-renderPlot({
        if(input$typeX == "p90X" & input$typeY != "p90Y"){
            scatterMaken90XDark(df = myData(),
                            percX=input$percX,
                            percY=input$percY)
        }else  if(input$typeX == "p90X" & input$typeY == "p90Y") {
            scatterMaken90Dark(df = myData(),
                           percX=input$percX,
                           percY=input$percY)
        }else  if(input$typeX != "p90X" & input$typeY == "p90Y") {
            scatterMaken90YDark(df = myData(),
                            percX=input$percX,
                            percY=input$percY)
        }else{
            scatterMakenDark(df = myData(),
                         percX=input$percX,
                         percY=input$percY)
        }
    }
    , height = 500, width = 750)
      
}

shinyApp(ui = ui, server = server)




