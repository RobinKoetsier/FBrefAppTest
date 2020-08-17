library(shiny)
library(dplyr)
library(ggplot2)
library(ggdark)
library(reactable)
library(GAlogger)
library(ggrepel)
library(glue)
source("HelpersFBREF.R")
ga_set_tracking_id("UA-175572271-1")
ga_set_approval(consent = TRUE)
ELCL <- readRDS("fbrefdata.rds")


ChoicesList <- colnames(ELCL)[c(3:55)]
ChoicesList <- sort(ChoicesList)
ui <- fluidPage(
    
    # Application title
    titlePanel("Create your own FBref scatter plot - A Shiny app by @RobinWilhelmus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(inputId = "Competition",
                             label = "Select Competition(s):",
                             choices = c("Champions League" = "Champions League",
                                         "Europa League" = "Europa League"),
                             selected = "Champions League"),
            radioButtons("typeX", "X Axis:",
                         c("Sum" = "normX",
                           "per 90" = "p90X")),
            
            radioButtons("typeY", "Y Axis:",
                         c("Sum" = "normY",
                           "per 90" = "p90Y")),
            
           
          
   
                
                selectInput('x', 'X', 
                          
                            selected = "Progressive Passes",
                            choices = ChoicesList, multiple=FALSE, selectize=TRUE),
            
          
                selectInput('y', 'Y', 
                            selected = "Assists",
                            choices = ChoicesList, multiple=FALSE, selectize=TRUE),
            
           
         
     
            

            sliderInput("minNinety", "Minimum number of 90s:",
                        min = 1, max = 5, value = 3
            ),
            numericInput("percX", "See label above certain percentile X:", 99, min = 50, max = 100),
            numericInput("percY", "See label above certain percentile Y:", 99, min = 50, max = 100)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", 
                                 h4("", align = "center"),
                                 
                                 plotOutput("plot2"),
                                tableOutput("myTable")),
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
    
    myData <- reactive({
         
        filterData(df = ELCL,
                   Xx = input$x,
                   Yy=input$y,
                   minNinety = input$minNinety,
                   comp = input$Competition)
        
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
            scatterMaken(df = myData(),
                           percX=input$percX,
                           percY=input$percY)
        }
        }
        , height = 400, width = 600)
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
    , height = 400, width = 600)
      
}

shinyApp(ui = ui, server = server)




