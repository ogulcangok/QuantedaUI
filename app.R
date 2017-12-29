

library(shiny)
library(quanteda)
library(readtext)
library(SnowballC)
library(ggplot2)
library(dplyr)
library("spacyr")


Sys.setlocale('LC_ALL','C')

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Insert App Name Here"),
  #Horizontal line ----
  tags$hr(),
  
  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
    # Input: Select a file ----
   
    
   
    # Sidebar panel for inputs ----
   # sidebarPanel(
      
     
      
      
     
    #),
    
      #Main panel for displaying outputs ----
      mainPanel(
      
        fileInput("file1", "Choose CSV File", multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      # Output: Data file ----
      tabsetPanel(
        tabPanel("Data",tableOutput("contents")),
                 
        tabPanel("Explore",
                 selectInput("plotMenu", "Select corpus filter", multiple = F,selectize = F,choices = c("Token","Read")),
                 tabsetPanel(tabPanel("Data",
                 
                 h3("Top 10 descending"),
                 tableOutput("calculateHeadDesc"),
                 h3("Top 10 ascending"),
                 tableOutput("calculateHead")),
                 tabPanel("Graph",
                 plotOutput("plotToken")),
                 
                 tabPanel("Text",
                          
                          numericInput("viewText","Enter text no",0),
                          actionButton("view","View Text"),
                          
                          checkboxInput("showText","Show/Hide Text",value = TRUE),
                   
                   conditionalPanel(
                   condition = "input.showText == true",
                   tableOutput("textView")))
                 
                 )),
                 
        tabPanel("Summary",
               
                 textInput("notes","Add Corpus Notes"),
                 
                 #action button to create corpus
                 column(12,
                        
                 column(2,actionButton("addNotes","Add Notes")),
                 
                 column(4,checkboxInput("metadisp","Display Metadata",value = FALSE))
                        ),
                 conditionalPanel(
                   condition = "input.metadisp == true", 
                   h1("Metadata"),
                   tableOutput("metaInfo")
                 ),
                 h1("Summary"),
                 tableOutput("summary"))
        
      
      )
      
      
    )
    
  )#end of side bar layout


# Define server logic to read selected file ----
server <- function(input, output) {
  
  readData <- reactive({
    
    aidata <- readtext(input$file1$datapath,text_field = "content")
    cat(input$plotMenu)
    aidata
    
  })
  
  
  createCorp <- reactive({
    
    aicorp <- corpus(readData())
    aicorp
  })
  
    output$contents <- renderTable({
       req(input$file1)
      readData()
      
       })
    observeEvent(input$addNotes,{
      
      aicorp <- createCorp()
      metacorpus(aicorp, "notes") <- input$notes
      #aicorp
      output$metaInfo <- renderTable(metacorpus(aicorp,"notes"))
      
      
     
    })
   
   
   calculateRead <- reactive({
     
     aicorp <- createCorp()
     fk <- textstat_readability(aicorp, "Flesch.Kincaid")
     docvars(aicorp, "fk") <- fk
     tokenInfo <-summary(aicorp, showmeta = T)
     tokenInfo
   })
   
   output$calculateHeadDesc <- renderTable({
     if(input$plotMenu == "Token"){
     head(arrange(calculateRead(),  desc(Tokens) ), n=10)
     }
     else {head(arrange(calculateRead(),  desc(fk) ), n=10)}
     
     
   })
   
   output$calculateHead <- renderTable({
     if(input$plotMenu == "Token"){
       head(arrange(calculateRead(),Tokens ), n=10)
     }
     else {head(arrange(calculateRead(),fk ), n=10)}
     
     
   })
    
    output$plotToken <- renderPlot({
      if(input$plotMenu == "Token"){
      ggplot(calculateRead(), aes(x =as.factor(year), y=Tokens)) + geom_boxplot()
      }  
      else {ggplot(calculateRead(), aes(x =as.factor(year), y=fk)) + geom_boxplot()}
    
     })
    
    
    observeEvent(input$view,{
     if(input$viewText > 0)
     {
      output$textView <- renderTable(texts(createCorp())[input$viewText])
     }
      else{
        output$textView <- renderTable()
      }
    })
   
    
    
    
    output$summary <- renderTable({
      
      aicorp <- createCorp()
      summary(aicorp)
    })
  
}



# Create Shiny app ----
shinyApp(ui, server)
