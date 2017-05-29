
library(shiny)
library(shinyjs)

library(shinydashboard)


header <- dashboardHeader()

sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem("TCGA-COADREAD_deseq", tabName = "TCGA-COADREAD_deseq", icon = icon("dashboard")),
    menuItem("ANNOVAR", tabName = "ANNOVAR", icon = icon("dashboard"))
  )
)

body <-   dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "TCGA-COADREAD_deseq",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "TCGA-KICH",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { }

shinyApp(ui, server)







