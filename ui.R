library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
    tags$a(href='javascript:void',
           id=inputId,
           class='action-button',
           ...)
}

fluidPage(
    titlePanel("MovieLens Explorer"),
    fluidRow(
        column(3,
               wellPanel(
                   h4("Filter"),
                   sliderInput("reviews", "Minimum number of reviews",
                               1,34400,1,step =100),
                   sliderInput("year", "Year released",1915, 2008, value = c(1915,2008),
                               sep = ""),
                   selectInput("genre", "Genre (a movie can have multiple genres)",
                               c("All", "Action", "Adventure", "Animation", "Comedy",
                                 "Crime", "Documentary", "Drama","Fantasy",
                                 "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
                                 "Thriller", "War", "Western")
                   ),
               ),
        ),
        column(9,
               ggvisOutput("plot1"),
               wellPanel(
                   span("Number of movies selected:",
                        textOutput("n_movies")
                   )
               )
        )
    )
)
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/#targetText=The%20easiest%20way%20to%20turn,app%20including%20server%20administration%20tools.