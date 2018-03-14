library(shiny)

load("ches_parties.Rdata")
load("ess_parties.Rdata")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Match Party Names"),
   shiny::selectInput("country", label = NULL, choices = ches_parties$country),
   # Sidebar with a slider input for number of bins 
   fluidRow(
       column(width = 6,
              DT::dataTableOutput("ches")
       ),
       column(width = 6,
              DT::dataTableOutput("ess")
       ),
       DT::dataTableOutput("ches_out")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$ches <- DT::renderDataTable({
    ches_parties[ches_parties$country == input$country, ]
  })
  
  output$ess <- DT::renderDataTable({
    ess_parties[ess_parties$country == input$country, ]
  })
  
  output$ches_out <- DT::renderDataTable({
    ches_parties[input$ches_rows_selected[1], ] %>%
      mutate(vote_id = ess_parties$vote_id[ess_parties$country == input$country & input$ches_rows_selected[1]])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

