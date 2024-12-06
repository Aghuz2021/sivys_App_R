# Cargar librerías
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(bslib)
library(dplyr)
library(reactable)
library(tidyr)


archivo <- read.csv("C://Users//Usuario//Desktop//APP_Syvis//bd//sivys_bd.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
archivo$dolares <- as.numeric(archivo$precio_dolares)

#View(archivo)

# Localidades de Tres de Febrero
localidades <- unique(archivo$nombre)
fechas_formateadas <- unique(archivo$fecha)
zonas_agrupadas <- unique(archivo$zona_agrupada)

# Cargar UI y Server
source("ui/header.R")
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
  header = headerUI,
  sidebar = sidebarUI,
  body = bodyUI
)

# Server
server <- function(input, output, session) {
  observe({
    showModal(modalDialog(
      title = "Bienvenidos",
      "Bienvenidos a la aplicación de análisis del mercado inmobiliario de Tres de Febrero. Explora las distintas opciones para ver la información sobre ventas y alquileres de propiedades.👋",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })

  source("plots/plot_venta_Casa.R", local = TRUE)
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)













