library(magrittr)
library(lubridate)
library(shiny)


source('app_data.R')
source('app_funs.R')



# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    .subtitle {
                    font-size: 16px;
                    color: #666;
                    margin-bottom: 10px;
                    }
                    .content {
                    margin-top: 10px;
                    }
                    "))
    ),
  titlePanel("Bigfoot Sightings in USA"),
  div("A Dataset of Reported Sightings from 1950 to 2021", class = "subtitle"),
  fluidRow(
    column(
      width = 6
      )
    ),
  fluidRow(
    column(
      width = 6,
      reactable::reactableOutput("tabela", height = "840px")
      ),
    column(
      width = 6,
      highcharter::highchartOutput("grafico", height = "420px"),
      leaflet::leafletOutput("mapa", height = "420px")
    )
  )

)

server <- function(input, output, session) {

  output$grafico <- highcharter::renderHighchart({
    graph2(bf_data)
  })

  output$mapa <- leaflet::renderLeaflet({
    map_bigfoot(bf_data)
  })

  output$tabela <- reactable::renderReactable({
    table(bf_data)
  })
}

shinyApp(ui, server)



