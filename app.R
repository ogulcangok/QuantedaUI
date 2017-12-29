library(shiny)
library(quanteda)
library(readtext)
library(SnowballC)
library(ggplot2)
library(dplyr)
library("spacyr")
library(DT)

Sys.setlocale('LC_ALL', 'C')

# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Insert App Name Here"),
                #Horizontal line ----
                tags$hr(),
                #Creating the main panel
                mainPanel(
                  #Adding a file input. This input can get multiple files and file types text or csv.
                  fileInput("file1","Choose CSV File",multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                  #Creating First main tab DATA.
                  tabsetPanel(
                    tabPanel("Data",
                       textInput("notes", "Add Corpus Notes"),#Add a text input for adding some notes to metadata
                      column(12,#create a column with a width 12 and assign each 3 widths an action button and two checkbox
                        column(2, actionButton("addNotes", "Add Notes")),
                        column(3, checkboxInput("metadisp", "Display Metadata", value = FALSE)),
                        column(3, checkboxInput("summdisp", "Display Summary", value = FALSE))),
                      
                      conditionalPanel(#checks if metadisp or summdisp is selected. If so shows the corresponding data
                        condition = "input.metadisp == true",
                        h1("Metadata"),
                        tableOutput("metaInfo")),
                      conditionalPanel(
                        condition = "input.summdisp == true",
                        h1("Summary"),
                        tableOutput("summary")),
                        tableOutput("contents")),
                    
                    tabPanel("Explore",#Second main tab EXPLORE.
                      selectInput("plotMenu","Select corpus filter",multiple = F,selectize = F,choices = c("Token", "Read")),#Select a filter from a combobox
                      
                      tabsetPanel(#Explore has many subtabs. Data tab shows the top and blow 10 of the given filter.
                        tabPanel("Data",
                         h3("Top 10 descending"),
                         tableOutput("calculateHeadDesc"),
                         h3("Top 10 ascending"),
                         tableOutput("calculateHead")),
                        tabPanel("Graph",#Graph tab gives the boxplot of the given filter.
                         plotOutput("plotToken")),
                        tabPanel("Text",#Text tab gives the full text which can be entered from the numericInput below.
                          numericInput("viewText", "Enter text no", 0),
                          actionButton("view", "View Text"),
                          checkboxInput("showText", "Show/Hide Text", value = TRUE),#I added an check box to enable and disable the text.
                          conditionalPanel(#again checks if it is enabled or not if so show the text
                            condition = "input.showText == true",
                            tableOutput("textView"))),
                        tabPanel("Subset",#Subset tab creates subsets of the given year. At this stage i give the years but with an algorithm we can pick the years and insert 
                                 #to the combo box.
                          selectInput("selectYear","Select Year",multiple = F,selectize = F,choices = c("2017", "2016", "2015", "2014")),
                          actionButton("createSubset", "Create Subset"),
                          tableOutput("showSubset")),
                        tabPanel("Concordence",#Concordence tab draws a graph, which gives info about a word in all corpuses.
                          column(3,textInput("keyWord", "Enter Your Key Word", width = 150)),
                          column(3,textInput("keyWord2", "Enter Your Key Word", width = 150)),
                          column(3,actionButton("save", "Save Subset")),
                          plotOutput("concordencePlot")))),
                        tabPanel("Tokenize")#TODO
                    
                    
                    
                    
                  )#end of main tabsetPanel
                  
                  
                ))#end of main layout


# Define server logic to read selected file ----
server <- function(input, output) {
 #Througout the server reactive is used maby times. Reactive stands for static in java. Simply it enables us to use the function anywhere we want.
  #All functions are from the downloaded packages. (non vanilla r)
  
   readData <- reactive({#Main function to read data from the csv
    aidata <- readtext(input$file1$datapath, text_field = "content")
    # cat(input$plotMenu)
    aidata
    
    
    
  })
  
  
  createCorp <- reactive({#creating the corpus from the read data.
    aicorp <- corpus(readData())
    aicorp
  })
  
  output$contents <- renderTable({#outputting the csv file as a table
    req(input$file1)
    readData()
    
  })
  observeEvent(input$addNotes, {#action Handler for adding notes the to metada 
    aicorp <- createCorp()
    metacorpus(aicorp, "notes") <- input$notes
    #aicorp
    output$metaInfo <- renderTable(metacorpus(aicorp, "notes"))
    })
  
  calculateRead <- reactive({#this is reactive because output of this func. will be used througout the whole server.
    aicorp <- createCorp()
    fk <- textstat_readability(aicorp, "Flesch.Kincaid")
    docvars(aicorp, "fk") <- fk
    tokenInfo <- summary(aicorp, showmeta = T)
    tokenInfo
  })
  
  output$calculateHeadDesc <- renderTable({#simple if else. if input is else calculate tokens.
    if (input$plotMenu == "Token") {
      head(arrange(calculateRead(),  desc(Tokens)), n = 10)
    }
    else {
      head(arrange(calculateRead(),  desc(fk)), n = 10)
    }
    })
  
  output$calculateHead <- renderTable({#outputting the result of the calculateHead() to a table
    if (input$plotMenu == "Token") {
      head(arrange(calculateRead(), Tokens), n = 10)
    }
    else {
      head(arrange(calculateRead(), fk), n = 10)
    }
    })
  
  output$plotToken <- renderPlot({#and drawing the plot of the calculateHead()
    if (input$plotMenu == "Token") {
      ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot()
    }
    else {
      ggplot(calculateRead(), aes(x = as.factor(year), y = fk)) + geom_boxplot()
    }
    })
  
  observeEvent(input$view, {#displaying the text to Text tab
    if (input$viewText > 0)
    {
      output$textView <- renderTable(texts(createCorp())[input$viewText])
    }
    else{
      output$textView <- renderTable()
    }
  })
  
  createSubset <- reactive({#reactive method to create subsets
    aiSet <- corpus_subset(createCorp(), year == input$selectYear)
    aiSet
  })
  
  output$showSubset <- renderPrint({
    createSubset()
  })
  
  createKW <- reactive({#concordence tab functions
    options(width = 200)
    scikw <- kwic(createSubset(), input$keyWord)
    scikw
  })
  
  output$concordencePlot <- renderPlot({#graphs of the concordence tab
    if (input$keyWord2 == "")
    {
      textplot_xray(createKW(), sort = T)
    }
    else
    {
      textplot_xray(
        kwic(createSubset(), input$keyWord),
        kwic(createSubset(), input$keyWord2),
        sort = T
      ) +
        aes(color = keyword) + scale_color_manual(values = c("blue", "red"))
    }
    
  })
  
  saveKW <- reactive({#reactive function to save the current subset as a .rda file to the working directory.
    KWsubset <-
      corpus_subset(createCorp(),
                    docnames(createCorp()) %in% createKW()$docname)#subsets the documents of which names match the kwic docs(home)
    save(KWsubset, file = "kwsubset.rda")
  })
  observeEvent(input$save, {
    saveKW()
  })
  
  output$summary <- renderTable({#this func. is displaying the summary of the data.
    aicorp <- createCorp()
    
    summary(aicorp)
    
  })
  }
# Create Shiny app ----
shinyApp(ui, server)
