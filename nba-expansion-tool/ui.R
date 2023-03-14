library('shiny')
library('shinyWidgets')

shinyUI(fluidPage(
  titlePanel("NBA Expansion Tool | Build Your Own Scenario"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("config_in", "Configuration", choices=c("Conferences", "Divisions"),
                  options = list(`actions-box` = TRUE), multiple = T)
    ),
    mainPanel(
      h3("Test")
    )
  )

))
