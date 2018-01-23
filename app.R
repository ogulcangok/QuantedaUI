library(shiny)
library(quanteda)
library(readtext)
library(SnowballC)
library(ggplot2)
library(dplyr)
library("spacyr")
library(DT)
library(shinyBS)
library(lubridate)
library(tidyverse)


Sys.setlocale('LC_ALL', 'C')
options(shiny.maxRequestSize=40*1024^2)



# Define UI for data upload app ----
ui <- fluidPage(# App title ----
                titlePanel("Insert App Name Here"),
                #Horizontal line ----
                tags$hr(),
                #Creating the main panel
                mainPanel(
                  
                  #Adding a file input. This input can get multiple files and file types text or csv.
                  
                  
                  column(12,
                         column(4,
                                fileInput("file1","Choose CSV File",multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))),
                         
                         column(4,actionButton("help","Help"))),
                  
                  
                  #Creating First main tab DATA.
                  tabsetPanel(
                    tabPanel("Data",
                             
                             textInput("notes", "Add Corpus Notes"),#Add a text input for adding some notes to metadata
                             column(12,#create a column with a width 12 and assign each 3 widths an action button and two checkbox
                                    column(2, actionButton("addNotes", "Add Notes")),
                                    column(3, checkboxInput("metadisp", "Display Metadata", value = FALSE)),
                                    column(3, checkboxInput("summdisp", "Display Summary", value = FALSE)),
                                    column(3,actionButton("helpData", "Help",width = 150))),
                             
                             conditionalPanel(#checks if metadisp or summdisp is selected. If so shows the corresponding data
                               condition = "input.metadisp == true",
                               h1("Metadata"),
                               tableOutput("metaInfo")),
                             conditionalPanel(
                               
                               condition = "input.summdisp == true",
                               h1("Summary"),
                               
                               DT::dataTableOutput("summary")
                               
                               
                             ),
                             tableOutput("contents")),
                    
                    tabPanel("Explore",#Second main tab EXPLORE.
                             
                             
                             tabsetPanel(#Explore has many subtabs. Data tab shows the top and blow 10 of the given filter.
                               
                               tabPanel("Graph",#Graph tab gives the boxplot of the given filter.
                                        column(12,
                                               column(5,selectInput("plotMenu","Select corpus filter",multiple = F,selectize = F,choices = c("Token", "Read"))),
                                               column(5,actionButton("helpGraph", "Help",width = 150))),#Select a filter from a combobox
                                        plotOutput("plotToken",dblclick = "plotTokenDbl")
                               ),
                               
                               
                               tabPanel("Concordence",#Concordence tab draws a graph, which gives info about a word in all corpuses.
                                        tabsetPanel(
                                          tabPanel("Lexical Plot",
                                                   column(12,
                                                          column(8,selectInput("selectYear","Select Year",multiple = F,selectize = F,choices = "")),
                                                          column(4,actionButton("helpLexical","Help"))),
                                                   tableOutput("showSubset"),
                                                   column(3,textInput("keyWord", "Enter Your Key Word", width = 150)),
                                                   column(3,textInput("keyWord2", "Enter Your Key Word", width = 150)),
                                                   
                                                   plotOutput("concordencePlot",dblclick = "concordencePlotDbl")),
                                          tabPanel("Text Data",
                                                   column(12,
                                                          column(8,textInput("saveName","Enter The Subset Name")),
                                                          column(4,actionButton("helpText","Help"))),
                                                   column(3,downloadButton("save", "Save Subset")),
                                                   column(3,textInput("concoInput","Enter a Key Word")),
                                                   tableOutput("concordenceTextOut"))
                                        ))
                               
                             )),
                    tabPanel("DFM",
                             tabsetPanel(
                               tabPanel("Plots",
                                        column(12,
                                               column(8,selectInput("plotSelect","Select Plot Type",c("Text Cloud","Frequency"),selected = NULL)),
                                               column(4,actionButton("helpPlot","Help"))),
                                        plotOutput("dfmPlot",dblclick = "dfmPlotDbl")        
                               ),
                               
                               tabPanel("Grouping",
                                        column(4,
                                               selectInput("groupSelect","Select Grouping Type",choices = c(""))),
                                        column(4,
                                               selectInput("plotSelectGroup", "Select Graph Type", c("Text Plot", "Baloon Plot"))),
                                        column(4,actionButton("helpGroup","Help")),
                                        checkboxInput("showSort","Show Info"),
                                        conditionalPanel(
                                          condition = "input.showSort == true",
                                          h1("Sort Summary"),
                                          tableOutput("sort")),
                                        plotOutput("groupPlot",dblclick = "groupPlotDbl")
                                        
                               ),
                               tabPanel("Frequency",
                                        h3("If the frequency is 0 it will give you your keyword's frequency."),
                                        column(6,
                                               sliderInput("frequencyInput","Change the number of the top Words",min = 0,max = 30,value = 0)),
                                        column(5,
                                               textInput("frequencyKeyWord","Enter Your Own Key Word")),
                                        column(4,actionButton("helpFreq","Help")),
                                        plotOutput("frequencyPlot",dblclick ="frequencyPlotDbl" )
                               ),
                               tabPanel("Keyness",
                                        column(4,actionButton("helpKey","Help")),
                                        column(4,
                                               selectInput("keynessYear1","Select Year","")),
                                        column(4,
                                               selectInput("keynessYear2", "Select Year", "")),
                                        plotOutput("keynessPlot",dblclick = "keynessPlotDbl")),
                               tabPanel("Dictionary",
                                        column(12,
                                               column(8, fileInput("file2","Upload Your Dictionary",multiple = FALSE,accept = ".dic")),
                                               column(4,actionButton("helpLexical","Help"))),
                                        tableOutput("dictTable"),
                                        plotOutput("dictPlot",dblclick = "dictPlotDbl")),
                               tabPanel("Similarity",
                                        
                                        column(12,
                                               column(4,
                                                      selectInput("similarityYear","Select Year","")),
                                               column(4,
                                                      selectInput("similaritySelect","Select Filter",c("Documents","Features")))),
                                        
                                        tableOutput("similarity")
                               ),
                               tabPanel("Clustering",
                                        column(4,
                                               selectInput("clusterYear","Select Year","")),
                                        column(4,
                                               selectInput("clusterSelect","Select Filter",c("documents","features"))),
                                        plotOutput("clustering")),
                               tabPanel("Topic Detection",
                                        
                                        plotOutput("topic")
                                        
                               )
                               
                               
                               
                             )        
                             
                             
                    )
                    
                    
                    
                    
                  )#end of main tabsetPanel
                  
                  
                ))#end of main layout


# Define server logic to read selected file ----
server <- function(input, output,session) {
  #Througout the server reactive is used maby times. Reactive stands for static in java. Simply it enables us to use the function anywhere we want.
  #All functions are from the downloaded packages. (non vanilla r)
  options(shiny.maxRequestSize=100*1024^2)
  
  
  modalConcor <- modalDialog("Plot",size = "l", plotOutput("modalConcorOut"))
  modalDfm <- modalDialog("Plot",size = "l",plotOutput("modalDfmOut"))
  modalGroup <-  modalDialog("Plot",size = "l",plotOutput("modalGroupOut"))
  modalFreq <- modalDialog("Plot",size = "l",plotOutput("modalFreqOut"))
  modalKey <- modalDialog("Plot",size = "l",plotOutput("modalKeyOut"))
  modalDict <- modalDialog("Plot",size = "l",plotOutput("modalDictOut"))
  modalHelpGeneral <- modalDialog("Help",size= "l",textOutput("modalHelpGeneralOut"))
  modalHelpData <- modalDialog("Help",size= "l",textOutput("modalHelpDataOut"))
  modalHelpGraph <- modalDialog("Help",size= "l",textOutput("modalHelpGraphOut"))
  modalHelpLexical <-modalDialog("Help",size= "l",textOutput("modalHelpLexicalOut"))
  modalHelpText <- modalDialog("Help",size= "l",textOutput("modalHelpTextOut"))
  modalHelpPlot <- modalDialog("Help",size= "l",textOutput("modalHelpPlotOut"))
  modalHelpGroup <- modalDialog("Help",size= "l",textOutput("modalHelpGroupOut"))
  modalHelpFreq <- modalDialog("Help",size= "l",textOutput("modalHelpFreqOut"))
  modalHelpKey <- modalDialog("Help",size= "l",textOutput("modalHelpKeyOut"))
  modalHelpDict <- modalDialog("Help",size= "l",textOutput("modalHelpDictOut"))
  
  
  
  observeEvent(input$help,{
    
    s <- "All plots are clickable. When clicked, it opens a pop up screen for detailed observatiion."
    
    output$modalHelpGeneralOut <- renderText(s)
    
    showModal(modalHelpGeneral)
    
    
    
  })
  observeEvent(input$helpData,{
    
    
    
    
    s <- "After you upload your CSV, in the data tab you will see your data in raw form.
    You can add notes to metadata by writing your note to the text field and clicking add notes button.
    By checking Display Metadata checkBox you can see your notes.
    If you check Display Summary box, you will be able to see corpuses in a data table. You can sort the corpuses
    by clicking the headers. If you click on a text, a pop-up screen will open and you will be able to read the corresponding text.
    
    "
    
    output$modalHelpDataOut <- renderText(s)
    
    
    
    showModal(modalHelpData)
    
    
  })
  
  observeEvent(input$helpGraph,{
    
    s <- "You can change the corpus filter for the plot."
    output$modalHelpGraphOut <- renderText(s)
    showModal(modalHelpGraph)
    
  })
  
  
  observeEvent(input$helpLexical,{
    
    s <- "You can select the subset year from the list. In order to see only one key word's plot,
    you can enter your key word to the rightmost textbox. If you want to see two at once, you can fill the other textbox.
    "
    
    output$modalHelpLexicalOut <- renderText(s)
    showModal(modalHelpLexical)
    
    
  })
  
  
  
  readData <- reactive({#Main function to read data from the csv
    
    aidata <- readtext(input$file1$datapath,text_field = "Text")
    
    
    
    
    aidata <- aidata%>%separate(Date,c("day","month","year"),remove = F)
    
    
    
  
    
    
    aidata
    
    
  })
  
  updateLists <- reactive({
    updateSelectInput(inputId = "groupSelect",session  ,choices = colnames(readData() ))
    updateSelectInput(inputId = "selectYear",session  ,choices = sort(unique(readData()$year)))
    updateSelectInput(inputId = "keynessYear1",session  ,choices = sort(unique(readData()$year)) )
    observe({
      listYear <- sort(unique(readData()$year))
      ch1 <- input$keynessYear1
      ch2 <- setdiff(listYear,ch1)
      
      
      
      updateSelectInput(inputId = "keynessYear2",session  ,choices = ch2)
    })
    updateSelectInput(inputId = "clusterYear",session  ,choices = sort(unique(readData()$year)))
    updateSelectInput(inputId = "similarityYear",session  ,choices = sort(unique(readData()$year)))
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
    tokenInfo <-summary(aicorp, showmeta = T)
    tokenInfo
  })
  
  modalPlot <- modalDialog("Token", size = "l", plotOutput("plotTokenModal"))
  output$plotToken <- renderPlot({#and drawing the plot of the calculateHead()
    
    if (input$plotMenu == "Token") {
      
      
      ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot()
      
      
    }
    else {
      ggplot(calculateRead(), aes(x = as.factor(year), y = fk)) + geom_boxplot()
    }
  })
  
  
  observeEvent(input$plotTokenDbl,{
    
    if (input$plotMenu == "Token")
    {
      output$plotTokenModal <- renderPlot(ggplot(calculateRead(), aes(x = as.factor(year), y = Tokens)) + geom_boxplot())}
    else{output$plotTokenModal <- renderPlot(ggplot(calculateRead(), aes(x = as.factor(year), y = fk)) + geom_boxplot())}
    showModal(modalPlot)
    
  })
  
  
  
  
  
  
  
  
  createSubset <- reactive({#reactive method to create subsets
    updateLists()
    aiSet <- corpus_subset(createCorp(), year == input$selectYear)
    aiSet
  })
  
  output$showSubset <- renderPrint({
    createSubset() 
  })
  
  output$concordenceTextOut <- renderTable(
    {
      kwic(createCorp(), input$concoInput , valuetype = "regex")
      
    }
  )
  
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
  
  
  observeEvent(input$concordencePlotDbl,{
    
    if (input$keyWord2 == "")
    {
      output$modalConcorOut <- renderPlot( textplot_xray(createKW(), sort = T))
    }
    else
    {
      output$modalConcorOut <- renderPlot(
        textplot_xray(
          kwic(createSubset(), input$keyWord),
          kwic(createSubset(), input$keyWord2),
          sort = T
        ) +
          aes(color = keyword) + scale_color_manual(values = c("blue", "red")))
    }
    showModal(modalConcor)
    
    
  })
  
  
  
  
  saveKW <- reactive({#reactive function to save the current subset as a .rda file to the working directory.
    
    
    KWsubset <-
      corpus_subset(createCorp(),
                    docnames(createCorp()) %in% createKW()$docname)#subsets the documents of which names match the kwic docs(home)
    KWsubset
  })
  
  
  
  output$save <- downloadHandler(
    
    filename = function() {
      paste(input$saveName,".rda")
    },
    content = function(file) {
      KWsubset <- saveKW()
      save(KWsubset, file = file)
    }
  )
  
  modalText <- modalDialog("Text View",size = "l", tableOutput("modalTextOut"))
  
  
  output$summary <-  DT::renderDataTable(selection = 'single',{#this func. is displaying the summary of the data.
    
    aicorp <- createCorp()
    
    s = input$summary_rows_selected
    if(length(s))
    {
      
      output$modalTextOut <- renderTable(texts(createCorp())[s])
      showModal(modalText)
      
      
      
      
      
    }
    fk <- textstat_readability(aicorp, "Flesch.Kincaid")
    docvars(aicorp, "fk") <- fk
    summary(aicorp)
    
    
    
    
  })
  
  createDFM <- reactive({
    
    ai.dfm <- dfm(createCorp(),remove = stopwords("SMART"),stem = T, remove_punct = T,remove_numbers =T)
    ai.dfm
    
    
  })
  
  createDFMW <- reactive({
    ai.dfmw <- dfm_weight(createDFM(),type = "tfidf")
    ai.dfmw
    
  })
  
  createDFMT <- reactive({
    
    ai.trim <- dfm_trim(createDFMW(),min_count = 100,max_count = 300,verbose = T)
  })
  
  getTopFeatures <- renderTable({
    
    topfeatures(createDFMW())
  })
  
  createTextCloud <- reactive({
    
    textplot_wordcloud(createDFMT(), max.words =Inf,  random.order = FALSE,
                       rot.per = .25, scale = c(2, 0.01), 
                       colors = RColorBrewer::brewer.pal(8,"Dark2"))
  })
  
  
  
  createFrequency <- reactive({
    aifr <- textstat_frequency(createDFMW(), n = 100)
    
  })
  
  frequencyDiag <- reactive({
    ggplot(createFrequency(), aes(x = feature, y = frequency)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  output$dfmPlot <- renderPlot({
    if(input$plotSelect == "Text Cloud")
    {
      createTextCloud()
    }
    else
    {frequencyDiag()}
    
  })
  
  observeEvent(input$dfmPlotDbl,{
    if(input$plotSelect == "Text Cloud")
    {
      output$modalDfmOut <- renderPlot(textplot_wordcloud(createDFMT(), max.words =Inf,  random.order = FALSE,
                                                          rot.per = .25, scale = c(0.9, 0.9), 
                                                          colors = RColorBrewer::brewer.pal(8,"Dark2")))
    }
    else
    {output$modalDfmOut <- renderPlot(ggplot(createFrequency(), aes(x = feature, y = frequency)) +
                                        geom_point() + 
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)))}
    showModal(modalDfm)
    
  })
  
  createGroup <- reactive({
    
    updateLists()
    ai.grp <- dfm(createCorp(), groups = input$groupSelect  , remove = stopwords("SMART"), remove_punct = TRUE)
    ai.grp
    
  })
  
  groupSort <- reactive ({
    
    dfm_sort(createGroup())[, 1:20]
  })
  output$sort <- renderTable({
    
    groupSort()
  })
  
  
  output$groupPlot <- renderPlot({
    if(input$plotSelectGroup == "Text Plot")
    {
      textplot_wordcloud(createGroup(), comparison = T,scale = c(0.9, 0.5))
    }
    else
    {
      
      aigr.trm <- dfm_trim(createGroup(), min_count = 500, verbose = T)
      dt <- as.table(as.matrix(aigr.trm))
      library("gplots")
      balloonplot(t(dt), main ="Words", xlab ="", ylab="",
                  label = FALSE, show.margins = FALSE,colsrt = 90,colmar = 6)    }
    
  })
  
  observeEvent(input$groupPlotDbl,{
    if(input$plotSelectGroup == "Text Plot")
    {
      output$modalGroupOut <- renderPlot(textplot_wordcloud(createGroup(), comparison = T,scale = c(0.9, 0.9)))
    }
    else
    {
      
      
      
      output$modalGroupOut <- renderPlot({ 
        
        
        aigr.trm <- dfm_trim(createGroup(), min_count = 500, verbose = T)
        dt <- as.table(as.matrix(aigr.trm))
        library("gplots")
        balloonplot(t(dt), main ="Words", xlab ="", ylab="",
                    label = FALSE, show.margins = FALSE,colsrt = 90,colmar = 3)
        
       
        }
        
        )
    }
    showModal(modalGroup)
    
  })
  
  filterTerm <- reactive({
    
    freq_grouped <- textstat_frequency(createDFMW(),n=input$frequencyInput,
                                       groups = "year")
    freq_grouped
    
  })
  
  filterKW <- reactive({
    fregr <- subset(filterTerm(), feature %in% input$frequencyKeyWord) 
    cat(input$frequencyKeyWord)
    fregr
    
  })
  
  output$frequencyPlot <- renderPlot({
    if(input$frequencyInput == 0)
    {
      freq_grouped <- textstat_frequency(createDFMW(),
                                         groups = "year")
      fregr <- subset(freq_grouped, feature %in% input$frequencyKeyWord) 
      ggplot(fregr,  aes(group, frequency)) +
        geom_point()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else
    {
      ggplot(data = filterTerm(), aes(x = nrow(filterTerm()):1, y = frequency)) +
        geom_point() +
        facet_wrap(~ group, scales = "free") +
        coord_flip() +
        scale_x_continuous(breaks = nrow(filterTerm()):1,
                           labels = filterTerm()$feature) +
        labs(x = NULL, y = "Relative frequency")
      
      
    }
    
    
  })
  
  observeEvent(input$frequencyPlotDbl,{
    
    if(input$frequencyInput == 0)
    {
      
      output$modalFreqOut <- renderPlot({
        freq_grouped <- textstat_frequency(createDFMW(),
                                           groups = "year")
        fregr <- subset(freq_grouped, feature %in% input$frequencyKeyWord) 
        ggplot(fregr,  aes(group, frequency)) +
          geom_point()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))})
    }
    else
    {
      output$modalFreqOut <- renderPlot({
        ggplot(data = filterTerm(), aes(x = nrow(filterTerm()):1, y = frequency)) +
          geom_point() +
          facet_wrap(~ group, scales = "free") +
          coord_flip() +
          scale_x_continuous(breaks = nrow(filterTerm()):1,
                             labels = filterTerm()$feature) +
          labs(x = NULL, y = "Relative frequency")})
      
      
    }
    showModal(modalFreq)
    
    
  })
  
  
  
  output$keynessPlot <- renderPlot({
    updateLists()
    ai.sub <- corpus_subset(createCorp(), 
                            year %in% c(input$keynessYear1, input$keynessYear2))
    
    ai.subdfm <- dfm(ai.sub, groups = "year", remove = stopwords("english"), 
                     remove_punct = TRUE)
    result_keyness <- textstat_keyness(ai.subdfm, target = input$keynessYear1)
    
    
    textplot_keyness(result_keyness) 
    
  })
  
  observeEvent(input$keynessPlotDbl,{
    updateLists()
    output$modalKeyOut <- renderPlot({
      ai.sub <- corpus_subset(createCorp(), 
                              year %in% c(input$keynessYear1, input$keynessYear2))
      
      ai.subdfm <- dfm(ai.sub, groups = "year", remove = stopwords("english"), 
                       remove_punct = TRUE)
      result_keyness <- textstat_keyness(ai.subdfm, target = input$keynessYear1)
      
      
      textplot_keyness(result_keyness) 
      
      
    })
    
    showModal(modalKey)
    
  })
  
  createDict <- reactive({
    
    dict <- input$file2$datapath
    
    
    myDict <- dictionary(file= dict, format = "LIWC")
    
    ai.dict <- dfm(createCorp(), groups = "year",  remove = stopwords("english"), remove_punct = TRUE, dictionary =myDict)
    topfeatures(ai.dict)
    
    dt <- as.table(as.matrix(ai.dict))
    dt
  })
  
  output$dictPlot <- renderTable({
    createDict()
  })
  
  output$dictPlot <- renderPlot({
    
    library("gplots")
    balloonplot(t(createDict()), main ="Words", xlab ="", ylab="",
                label = FALSE, show.margins = FALSE)
    
    
  })
  
  observeEvent(input$dictPlotDbl,{
    
    output$modalDictOut <- renderPlot({
      
      library("gplots")
      balloonplot(t(createDict()), main ="Words", xlab ="", ylab="",
                  label = FALSE, show.margins = FALSE)
      
      
    })
    showModal(modalDict)
  })
  
  
  calculateSimilarity <- reactive({
    updateLists()
    ai2016 <- corpus_subset(createCorp(), year==input$similarityYear)
    ai2016dfm <- dfm(ai2016, stem = T, remove = stopwords("english"), remove_punct=T)
    a <- textstat_dist(dfm_weight(ai2016dfm, "tfidf"), margin="documents", method="euclidean")
    table(a)
    
    
    
  })
  
  calculateSimilartyFeatures <- reactive({
    
    ai.trm <- dfm_trim(createDFMW(), min_count = 200, max_count = 300,  verbose = T)
    d <- textstat_simil(dfm_weight(ai.trm, "tfidf"), margin="features", method="cosine")
    table(d)
    
  })
  
  output$similarity <- renderTable({
    
    if(input$similaritySelect == "Documents")
    { calculateSimilarity()}
    else
    {calculateSimilartyFeatures()}
  })
  
  
  output$clustering <- renderPlot({
    
    
    if(input$clusterSelect == "documents")
    {
      updateLists()
      ai2016 <- corpus_subset(createCorp(), year==input$clusterYear)
      ai2016dfm <- dfm(ai2016, stem = T, remove = stopwords("english"), remove_punct=T)
      d <- textstat_simil(dfm_weight(ai2016dfm, "tfidf"), margin="documents", method="cosine")
      library(dendextend)
      hc_res <- hclust(d, method = "ward.D")
      dend <- as.dendrogram(hc_res)
      plot(dend, 
           horiz =  TRUE,  nodePar = list(cex = .007))
      
      
    }
    else{
      
      ai.trm <- dfm_trim(createDFMW(), min_count = 200, max_count = 300,  verbose = T)
      d <- textstat_simil(dfm_weight(ai.trm, "tfidf"), margin="features", method="cosine")
      library(dendextend)
      hc_res <- hclust(d, method = "ward.D")
      dend <- as.dendrogram(hc_res)
      plot(dend, 
           horiz =  TRUE,  nodePar = list(cex = .007))
    }
    
    
  })
  
  output$topic <- renderPlot({
    
    library(topicmodels)
    
    myLDAfit20 <- LDA(convert(createDFMT(),to="topicmodels"),k= 20)
    get_terms(myLDAfit20,5)
    
    
    
    
    
    
    
    
  })
  
  
}
# Create Shiny app ----
shinyApp(ui, server)
