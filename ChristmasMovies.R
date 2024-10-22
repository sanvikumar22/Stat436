library(DT)
library(shiny)
library(tidyverse)
library(stringr)
library(shinyWidgets)
library(tm)
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(readr)

movies <-
  read_csv(
    "https://raw.githubusercontent.com/sanvikumar22/Stat436/refs/heads/main/christmas_movies.csv"
  )

bar_plot <- function(in_movies) {
  genre_counts <- in_movies %>%
    separate_rows(genre, sep = ", ") %>%
    count(genre) %>%
    arrange(desc(n))  # Order by count
  
  ggplot(genre_counts, aes(x = reorder(genre,-n), y = n)) +
    geom_bar(stat = "identity",
             fill = "skyblue",
             color = "black") +
    theme_minimal() +
    labs(title = "Frequency of Genres", x = "Genre", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


scatter <- function(movies, in_movies) {
  ggplot() +
    geom_point(
      data = movies,
      aes(x = imdb_rating, y = votes, color = rating),
      size = 1,
      alpha = 0.2
    ) +
    geom_point(data = in_movies,
               aes(x = imdb_rating, y = votes, color = rating),
               size = 3) +
    labs(title = "IMDB Ratings vs Votes", x = "IMDB Rating", y = "Votes") +
    theme_minimal()
}

generate_wordcloud <- function(movies) {
  docs <- Corpus(VectorSource(movies$description))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)
  df <- df[1:min(100, nrow(df)),]
  christmas_colors <-
    c("red",
           "green",
           "gold",
           "darkred",
           "darkgreen",
           "lightgreen",
           "lightcoral")
           
  wordcloud2(
    df,
    size = 0.5,
    color = sample(christmas_colors, size = nrow(df), replace = TRUE),
    backgroundColor = "white"
  )
}

table <- function(in_movies) {
  movies_with_images <- in_movies %>%
    mutate(img_html = paste0('<img src="', img_src, '" width="100" height="150" />')) %>%
    rename(
      Image = img_html,
      Title = title,
      Rating = rating,
      Description = description,
      'Release Year' = release_year,
      Genre = genre,
      Director = director,
      Stars = stars,
      'IMDB Rating' = imdb_rating,
      Votes = votes,
      Gross = gross
    )
  
  movies_with_images <- movies_with_images %>%
    select(
      Image,
      Title,
      Rating,
      Description,
      'Release Year',
      Genre,
      Director,
      Stars,
      'IMDB Rating',
      Votes,
      Gross
    )
  
  datatable(
    movies_with_images,
    options = list(autoWidth = TRUE),
    escape = FALSE,
    width = "100%"
  ) %>%
    formatStyle("Image", target = "cell")
}

reset_selection <- function(x, brush) {
  if (is.null(brush))
    return(rep(TRUE, nrow(x)))
  brushedPoints(x, brush, allRows = TRUE)$selected_
}


ui <- fluidPage(
  titlePanel("Welcome to the Christmas Movie Analysis Visualization"),
  p(
    "Interactively explore Christmas movies by first filtering by genre/rating and then dragging to brush over points in the scatterplot."
  ),
  fluidRow(column(
    4,
    sidebarPanel(
      id = "sidebar",
      selectInput(
        "genre",
        "Genre",
        choices = unique(unlist(strsplit(movies$genre, ", "))),
        multiple = TRUE
      ),
      selectInput(
        "rating",
        "Rating Selection:",
        choices = unique(movies$rating),
        #selected = unique(movies$rating),
        multiple = TRUE
      )
    )
  ),
  column(
    8,
    plotOutput(
      "scatterplot",
      brush = brushOpts("plot_brush"),
      height = 300
    )
  )),
  
  fluidRow(column(6,
                  plotOutput("barplot", height = 300)),
           
           column(
             6,
             wordcloud2Output("wordcloud", height = 300)
           )),
  
  fluidRow(column(12,
                  dataTableOutput("table")))
)


server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(movies)))
  
  current_data <- reactive({
    movies %>%
      filter(str_detect(genre, paste(input$genre, collapse = "|")),
             rating %in% input$rating | is.na(rating))
  })
  
  observeEvent(input$plot_brush, {
    selected(reset_selection(current_data(), input$plot_brush))
  })
  
  observeEvent(c(input$genre, input$rating), {
    selected(reset_selection(movies, input$plot_brush))
  })
  
  output$scatterplot <- renderPlot({
    scatter(movies, current_data())
  })
  
  output$barplot <- renderPlot({
    bar_plot(current_data()[selected(),])
  })
  
  output$wordcloud <- renderWordcloud2({
    if (is.null(input$plot_brush)) {
      generate_wordcloud(movies)
    } else {
      generate_wordcloud(current_data()[selected(),])
    }
  })
  
  output$table <- renderDataTable({
    if (is.null(input$plot_brush)) {
      table(movies)
    }
    else{
      table(current_data()[selected(),])
    }
  })
}

shinyApp(ui, server)
