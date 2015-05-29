library(googleVis)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estadísticas del índice de digitalización territorial"),
  
  fluidRow(
    
    
    column(4,
           radioButtons("tipo_t",
                        label = h4("Tipo de territorio"),
                        choices = list(
                          "Ciudades" = 1,
                          "Prrq. Rurales" = 2,
                          "Ambos" = 3
                        ),
                        selected = 1
           )
           
    ),
    
    column(4,
           sliderInput("habs_t",
                       label = h4("Rango de número de habitantes (0:100, 3:100 mil ,5:10 mill.)"),
                       min= 0,
                       max = 5,
                       step = 0.25,
                       value = c(0,5)
           )
           
    ),
    
    column(4,htmlOutput("dat_slider")),
    
    column(12,htmlOutput("titulo_idv3")),
    column(3,htmlOutput("valor_estad_idv3")),
    column(9,htmlOutput("grafico1")),
    column(12,htmlOutput("titulo_pob")),
    column(3,htmlOutput("valor_estad_pob")),
    column(9,htmlOutput("grafico2")),
    column(12,htmlOutput("titulo_top")),
    column(12,htmlOutput("grafico3")),
    column(12),   
    column(11,offset=1,htmlOutput("tabla_top"))    
  )
  
))
