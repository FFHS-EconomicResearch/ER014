#install.packages("shiny")
library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Airbnb-Preise Zürich"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Import Data ------
# library(data.table)
# ZH_listings <- fread('http://data.insideairbnb.com/switzerland/z%C3%BCrich/zurich/2023-09-24/data/listings.csv.gz')
#
# ZH_shiny <- ZH_listings %>%
#   mutate(kreis=factor(neighbourhood_group_cleansed,
#                       levels=c("Kreis 1","Kreis 2","Kreis 3","Kreis 4",
#                                "Kreis 5","Kreis 6","Kreis 7","Kreis 8",
#                                "Kreis 9","Kreis 10","Kreis 11","Kreis 12"
#                       )
#   ),
#   preis=readr::parse_number(price)
#   )
#class(tbl_shiny$preis)
# ZH_shiny %>%
#     select(preis) %>%
#     filter(preis<=2500) %>%
#     unlist() %>%
#     write_rds("ZH_data.rds")

# Load data ----
ZH_data <- read_rds("ZH_data.rds")


# Define server logic required to draw a histogram ----
server <- function(input, output) {


  output$distPlot <- renderPlot({

    x    <- unlist(ZH_data)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Airbnb-Preise in Zürich",
         main = "Histogramm Preisverteilung")

  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
