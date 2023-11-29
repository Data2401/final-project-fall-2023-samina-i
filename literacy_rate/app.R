#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# UI part of the Shiny app
ui <- fluidPage(
  titlePanel("Literacy Rate Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", choices = unique(LIT_rate_data$Country))
    ),
    mainPanel(
      plotlyOutput("gender_plot"),
      plotlyOutput("age_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$gender_plot <- renderPlotly({
    filtered_data <- LIT_rate_data %>%
      filter(Country == input$country, Gender %in% c("female", "male")) %>%
      group_by(Gender, Year) %>%
      summarise(Lit_mean = mean(na.omit(`Literacy rate`)))
    
    p <- ggplot(filtered_data, aes(x = Year, y = Lit_mean, color = Gender)) +
      geom_line() +
      geom_point() +
      labs(title = "Mean literacy rate by Gender", x = "Year", y = "Mean Literacy rate")
    
    ggplotly(p)
  })
  
  output$age_plot <- renderPlotly({
    filtered_data <- LIT_rate_data %>%
      filter(Country == input$country) %>%
      group_by(Age, Year) %>%
      summarize(mean_lit_rate = mean(`Literacy rate`))
    
    p <- ggplot(filtered_data, aes(y = mean_lit_rate, x = Year, fill = Age)) +
      geom_col(position = "dodge") +
      labs(title = "Literacy rate by Year", x = "Year", y = "Mean Literacy Rate")
    
    ggplotly(p)
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)

