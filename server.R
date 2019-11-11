library(ggvis)
library(dplyr)
movies1<-read.csv('movies1.csv')
function(input, output, session) {
    # Filter the movies, returning a data frame
    movies <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
        reviews <- input$reviews
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
        # Apply filters
        m <- movies1 %>%
            filter(
                freq >= reviews,
                year >= minyear,
                year <= maxyear,
            ) 
        
        # Optional: filter by genre
        if (input$genre != "All") {
            if(input$genre=="Action")
                m <- m %>% filter(Action==1)
            else if(input$genre=="Adventure")
                m <- m %>% filter(Adventure==1)
            else if(input$genre=="Animation")
                m <- m %>% filter(Animation==1)
            else if(input$genre=="Comedy")
                m <- m %>% filter(Comedy==1)
            else if(input$genre=="Crime")
                m <- m %>% filter(Crime==1)
            else if(input$genre=="Documentary")
                m <- m %>% filter(Documentary==1)
            else if(input$genre=="Drama")
                m <- m %>% filter(Drama==1)
            else if(input$genre=="Fantasy")
                m <- m %>% filter(Fantasy==1)
            else if(input$genre=="Horror")
                m <- m %>% filter(Horror==1)
            else if(input$genre=="Musical")
                m <- m %>% filter(Musical==1)
            else if(input$genre=="Mystery")
                m <- m %>% filter(Mystery==1)
            else if(input$genre=="Romance")
                m <- m %>% filter(Romance==1)
            else if(input$genre=="Sci-Fi")
                m <- m %>% filter(SciFi==1)
            else if(input$genre=="Thriller")
                m <- m %>% filter(Thriller==1)
            else if(input$genre=="War")
                m <- m %>% filter(War==1)
            else if(input$genre=="Western")
                m <- m %>% filter(Western==1)
        }
       
        m <- as.data.frame(m)
        m
    })
    
    # Function for generating tooltip text
    movie_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$title)) return(NULL)
        
        # Pick out the movie with this title
        movies1 <- isolate(movies())
        movie <- movies1[movies1$title == x$title, ]
        
        paste0("<b>", movie$title, "</b><br>",
               movie$year, "<br>",
                format(round(movie$rating,digits=2), big.mark = ",", scientific = FALSE)
        )
    }
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
       
        movies %>%
            ggvis(x = ~rating, y = ~freq) %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                          key := ~title) %>%
            add_tooltip(movie_tooltip, "hover") %>%
            add_axis("x", title = "Ratings") %>%
            add_axis("y", title = "Reviews",title_offset = 60) %>%
            set_options(width = 500, height = 500)
    })
    
    vis %>% bind_shiny("plot1")
    
    output$n_movies <- renderText({ nrow(movies()) })
}