#the app is best viewed in full screen

#Libraries
library(shiny)        #1.6.0
library(tidyverse)    #1.3.1
library(plotly)       #4.9.4

#Interface
ui <- fluidPage(
  titlePanel("Bacterial composition of Waternet's samples"),
  sidebarLayout(
    sidebarPanel(numericInput("numx", "Amount of reads (max = 527548)", value = 10000, min = 0, max = 527548),
                 sliderInput("num2", "Probability", value = 100, min = 0, max = 100),
                 numericInput("num3", "Minimal occurences", value = 7, min = 0, max = 100)),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Tables",
                 fluidRow(
                   column(width = 5, p(strong("Table of the pre sample")), tableOutput("table_pre")),
                   column(width = 5, p(strong("Table of the post sample")), tableOutput("table_post")))),
        
        tabPanel("Figures",
                 fluidRow(
                   column(width = 6, p(strong("Bar graph of the pre sample")), plotOutput("bar_graph_pre")),
                   column(width = 6, p(strong("Bar graph of the post sample")), plotOutput("bar_graph_post"))),
                 fluidRow(
                   column(width = 6, p(strong("Pie chart of the pre sample")), plotlyOutput("pie_chart_pre")),
                   column(width = 6, p(strong("Pie chart of the post sample")), plotlyOutput("pie_chart_post"))))
      ))))




#creates tables
server <- function(input, output, session) {
  output$table_pre <- 
    renderTable({
      #Import the data
      BLAST_pre_x <- read_csv(here::here("data_raw/output_blast_pre"), col_names = FALSE, n_max = input$numx)
      BLAST_pre_x <- rename(BLAST_pre_x, query = X1, probability = X2, name = X3)
      #Filter on probability (num2)
      BLAST_pre_filter <- BLAST_pre_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
      #Filter only the first 2 words in the name column
      BLAST_pre_filter <- word(BLAST_pre_filter, 1, 2) %>% as_tibble()
      BLAST_pre_filter <- rename(BLAST_pre_filter, name = value)
      #Count the amount per read classification 
      BLAST_pre_count <- BLAST_pre_filter %>% group_by(name) %>% count()
      #filter the rows on their occurrence (num3)
      BLAST_pre_count_filter <- BLAST_pre_count %>% filter(n >= input$num3)
      #show the data
      head(BLAST_pre_count_filter, n=20)
    })
  ######################################################################################################################  
  output$table_post <- 
    renderTable({
      #Import the data
      BLAST_post_x <- read_csv(here::here("data_raw/output_blast_post"), col_names = FALSE, n_max = input$numx)
      BLAST_post_x <- rename(BLAST_post_x, query = X1, probability = X2, name = X3)
      #Filter on probability (num2)
      BLAST_post_filter <- BLAST_post_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
      #Filter only the first 2 words in the name column
      BLAST_post_filter <- word(BLAST_post_filter, 1, 2) %>% as_tibble()
      BLAST_post_filter <- rename(BLAST_post_filter, name = value)
      #Count the amount per read classification 
      BLAST_post_count <- BLAST_post_filter %>% group_by(name) %>% count()
      #filter the rows on their occurrence (num3)
      BLAST_post_count_filter <- BLAST_post_count %>% filter(n >= input$num3)
      #show the data
      head(BLAST_post_count_filter, n=20)
    })
#Creating plots
  ###################################################################################################################### 
  output$bar_graph_pre <- renderPlot({
    #Import the data
    BLAST_pre_x <- read_csv(here::here("data_raw/output_blast_pre"), col_names = FALSE, n_max = input$numx)
    BLAST_pre_x <- rename(BLAST_pre_x, query = X1, probability = X2, name = X3)
    #Filter on probability (num2)
    BLAST_pre_filter <- BLAST_pre_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
    #Filter only the first 2 words in the name column
    BLAST_pre_filter <- word(BLAST_pre_filter, 1, 2) %>% as_tibble()
    BLAST_pre_filter <- rename(BLAST_pre_filter, name = value)
    #Count the amount per read classification 
    BLAST_pre_count <- BLAST_pre_filter %>% group_by(name) %>% count()
    #filter the rows on their occurrence (num3)
    BLAST_pre_count_filter <- BLAST_pre_count %>% filter(n >= input$num3)
    
    bar_graph_pre <- BLAST_pre_count_filter %>%
      ggplot(aes(x = name, y = n)) +
      geom_col(aes(fill = name), show.legend = FALSE) +
      theme(axis.text.x = element_text(angle=90)) +
      labs(x = "Species",
           y = "Amount of reads") + theme_minimal() +
      coord_flip()
    bar_graph_pre
  })
  ######################################################################################################################
  output$bar_graph_post <- renderPlot({
    #Import the data
    BLAST_post_x <- read_csv(here::here("data_raw/output_blast_post"), col_names = FALSE, n_max = input$numx)
    BLAST_post_x <- rename(BLAST_post_x, query = X1, probability = X2, name = X3)
    #Filter on probability (num2)
    BLAST_post_filter <- BLAST_post_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
    #Filter only the first 2 words in the name column
    BLAST_post_filter <- word(BLAST_post_filter, 1, 2) %>% as_tibble()
    BLAST_post_filter <- rename(BLAST_post_filter, name = value)
    #Count the amount per read classification 
    BLAST_post_count <- BLAST_post_filter %>% group_by(name) %>% count()
    #filter the rows on their occurrence (num3)
    BLAST_post_count_filter <- BLAST_post_count %>% filter(n >= input$num3)
    
    
    bar_graph_post <- BLAST_post_count_filter %>%
      ggplot(aes(x = name, y = n)) +
      geom_col(aes(fill = name), show.legend = FALSE) +
      theme(axis.text.x = element_text(angle=90)) +
      labs(x = "Species",
           y = "Amount of reads") + theme_minimal() +
      coord_flip()
    bar_graph_post
  })
  ######################################################################################################################  
  output$pie_chart_pre <- renderPlotly({
    #Import the data
    BLAST_pre_x <- read_csv(here::here("data_raw/output_blast_pre"), col_names = FALSE, n_max = input$numx)
    BLAST_pre_x <- rename(BLAST_pre_x, query = X1, probability = X2, name = X3)
    #Filter on probability (num2)
    BLAST_pre_filter <- BLAST_pre_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
    #Filter only the first 2 words in the name column
    BLAST_pre_filter <- word(BLAST_pre_filter, 1, 2) %>% as_tibble()
    BLAST_pre_filter <- rename(BLAST_pre_filter, name = value)
    #Count the amount per read classification 
    BLAST_pre_count <- BLAST_pre_filter %>% group_by(name) %>% count()
    #filter the rows on their occurrence (num3)
    BLAST_pre_count_filter <- BLAST_pre_count %>% filter(n >= input$num3)
    
    #Plot the pie chart
    pie_chart_pre <- plot_ly(data = BLAST_pre_count_filter, labels = ~name, values = ~n,
                             type = 'pie',
                             marker= list(names=names, line = list(color = "black", width = 0.8))) 
    pie_chart_pre
  })
  ######################################################################################################################
  output$pie_chart_post <- renderPlotly({
    #Import the data
    BLAST_post_x <- read_csv(here::here("data_raw/output_blast_post"), col_names = FALSE, n_max = input$numx)
    BLAST_post_x <- rename(BLAST_post_x, query = X1, probability = X2, name = X3)
    #Filter on probability (num2)
    BLAST_post_filter <- BLAST_post_x %>% filter(probability >= input$num2) %>% select(name) %>% as_vector()
    #Filter only the first 2 words in the name column
    BLAST_post_filter <- word(BLAST_post_filter, 1, 2) %>% as_tibble()
    BLAST_post_filter <- rename(BLAST_post_filter, name = value)
    #Count the amount per read classification 
    BLAST_post_count <- BLAST_post_filter %>% group_by(name) %>% count()
    #filter the rows on their occurrence (num3)
    BLAST_post_count_filter <- BLAST_post_count %>% filter(n >= input$num3)
    
    #Plot the pie chart
    pie_chart_post <- plot_ly(data = BLAST_post_count_filter, labels = ~name, values = ~n,
                              type = 'pie',
                              marker= list(names=names, line = list(color = "black", width = 0.8))) 
    pie_chart_post
  })  
  
}

shinyApp(ui, server)
