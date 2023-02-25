library(shiny)



source('app_funs.R')



# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      tags$h2("Bigfoot Sightings in USA")
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("grafico")
    ),
    column(
      width = 6,
      leaflet::leafletOutput("mapa"),
      reactable::reactableOutput("tabela")
    )
  )

)

server <- function(input, output, session) {

  output$grafico <- highcharter::renderHighchart({
    graph2(bf_data, Year, N_obs)
  })

  output$mapa <- leaflet::renderLeaflet({
    map_bigfoot(bf_data)
  })

  output$tabela <- reactable::renderReactable({
    table(bf_data)
  })
}

shinyApp(ui, server)



