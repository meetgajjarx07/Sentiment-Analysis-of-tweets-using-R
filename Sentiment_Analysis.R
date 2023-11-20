library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(DT)
library(tm)
library(wordcloud)

shinyApp(
  ui = fluidPage(
    titlePanel("Sentiment Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload Your Text File",accept = "text/plain"),
        sliderInput("wordfreq",
                    "Minimum Frequency:",
                    min = 1,  max = 50, value = 15),
        sliderInput("maxword",
                    "Maximum Number of Words:",
                    min = 1,  max = 300,  value = 100),
        actionButton("update","Create Word Cloud")
      ),
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Word List", 
                             h3("Top 10 Positive Words"),
                             DTOutput("tbpos"),
                             br(),
                             h3("Top 10 Negative Words"),
                             DTOutput("tbneg"),
                             br()),
                    tabPanel("Visualization", plotOutput("p_sentT")),
                    tabPanel("WordCloud",plotOutput("wcplot"))
        )
      )
    )
  ),
  server = function(input,output,session) {
    filedata <- reactive({
      infile <- input$file
      if (is.null(infile)){
        return(NULL)   
      }
      scan(infile$datapath, character(0), sep=".",quote=NULL)
      
    })
    
    output$tbpos <- DT::renderDataTable({
      
      if (is.null(input$file)){
        return(NULL)      
      }
      
      LookForKeyword <- c(input$keyword)
      
      df <- filedata()
      df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
      
      
      sentiments <- read.csv("afinn.csv", sep=",")
      labMT <- sentiments %>%
        select(word, value)
      
      ### Quick sentiment analysis
      
      allsentimentT <- df2 %>%  
        select(value) %>%
        unnest_tokens(word, value) %>%
        anti_join(stop_words) %>%
        inner_join(labMT, by = "word") %>%
        group_by(word) %>%
        summarize(sentiment = mean(value)) %>%
        arrange(desc(sentiment)) %>%
        mutate("sentiment2" = sentiment-0 )
      
      # Bind 10 most positive terms and 10 most negative terms
      
      topsent <- allsentimentT %>%
        top_n(10) 
      
      DT::datatable(topsent)
      
    })
    
    output$tbneg <- DT::renderDataTable({
      
      if (is.null(input$file)){
        return(NULL)      
      }
      
      LookForKeyword <- c(input$keyword)
      
      df <- filedata()
      df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
      
      
      sentiments <- read.csv("afinn.csv", sep=",")
      labMT <- sentiments %>%
        select(word, value)
      
      ### Quick sentiment analysis
      
      allsentimentT <- df2 %>%  
        select(value) %>%
        unnest_tokens(word, value) %>%
        anti_join(stop_words) %>%
        inner_join(labMT, by = "word") %>%
        group_by(word) %>%
        summarize(sentiment = mean(value)) %>%
        arrange(desc(sentiment)) %>%
        mutate("sentiment2" = sentiment-0 )
      
      # Bind 10 most positive terms and 10 most negative terms
      
      bottomsent <- head(arrange(allsentimentT,sentiment2), n = 10) 
      
      DT::datatable(bottomsent)
      
    })
    
    output$p_sentT <- renderPlot({
      
      if (is.null(input$file)){
        return(NULL)      
      }
      
      LookForKeyword <- c(input$keyword)
      
      df <- filedata()
      df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
      
      
      sentiments <- read.csv("afinn.csv", sep=",")
      labMT <- sentiments %>%
        select(word, value)
      
      ### Quick sentiment analysis
      
      allsentimentT <- df2 %>%  
        select(value) %>%
        unnest_tokens(word, value) %>%
        anti_join(stop_words) %>%
        inner_join(labMT, by = "word") %>%
        group_by(word) %>%
        summarize(sentiment = mean(value)) %>%
        arrange(desc(sentiment)) %>%
        mutate("sentiment2" = sentiment-0 )
      
      bottomsentT <- allsentimentT %>%
        top_n(-10) 
      topsentT <- allsentimentT %>%
        top_n(10) 
      sentimentT <- bind_rows(bottomsentT,topsentT) %>%
        arrange(desc(sentiment2)) %>%
        distinct() # remove duplicates
      sentimentT
      
      
      
      p_sentT <- ggplot(sentimentT, aes(x= reorder(word, -sentiment2), 
                                        y = sentiment2, 
                                        fill = sentiment2 > 0)) + #this is midpoint of labMT sentiment dictionary
        geom_col(show.legend = FALSE) +
        coord_flip() +
        ylab("sentiment") +
        xlab("word") + 
        scale_y_continuous(limits=c(-5, 5)) +
        scale_fill_manual(values=c("red","blue"))
      
      p_sentT
      
    })
    
    wc_data <- reactive({
      
      input$update
      
      isolate({
        
        withProgress({
          
          setProgress(message = "Processing")
          
          infile <- input$file

          wc_text <- readLines(infile$datapath)
          wc_corpus <- Corpus(VectorSource(wc_text))
          wc_corpus_clean <- tm_map(wc_corpus, tolower)
          wc_corpus_clean <-tm_map(wc_corpus_clean, removeNumbers)
          wc_corpus_clean <-tm_map(wc_corpus_clean, removeWords, stopwords())
          wc_corpus_clean <-tm_map(wc_corpus_clean, stripWhitespace)
          wc_corpus_clean <-tm_map(wc_corpus_clean, stemDocument)
          
        })
        
      })
      
    })
    
    wordlcloud_rep <- repeatable(wordcloud)
    
    output$wcplot <- renderPlot({
      
      withProgress({
        
        setProgress(message = "Creating WordCloud...")
        wc_corpus <- wc_data()
        
        wordcloud(wc_corpus, min.freq = input$wordfreq,max.words = input$maxword,
                  colors=brewer.pal(8, "Dark2"),random.order=FALSE,rot.per = .3)
        
      })
      
      
    })
    
    
  }
)
