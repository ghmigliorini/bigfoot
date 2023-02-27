library(shiny)


source('app_data.R')
source('app_funs.R')



# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Bigfoot Sightings in USA"),
  fluidRow(
    column(
      width = 6
    )
  ),
  fluidRow(
    column(
      width = 6,
      reactable::reactableOutput("tabela")
    ),
    column(
      width = 6,
      highcharter::highchartOutput("grafico"),
      leaflet::leafletOutput("mapa")
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



