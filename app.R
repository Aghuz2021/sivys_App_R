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
library(sf)

archivo <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//sivys_bd.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
archivo2 <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//ripte(data).csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
geojson_path <- "C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//zonas.geojson" # Cambiar por tu archivo
geojson_macrozonas <- "C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//macrozonas.geojson" # Cambiar por tu archivo

tres_de_febrero <- st_read(geojson_path)
macrozonas <- st_read(geojson_macrozonas)


archivo$dolares <- as.numeric(archivo$precio_dolares)


archivo$dolares_m2total <- gsub("\\s", "", archivo$dolares_m2total)  # Eliminar espacios
archivo$dolares_m2total <- gsub("\\.", "", archivo$dolares_m2total)  # Eliminar puntos (miles)
  archivo$dolares_m2total <- gsub(",", ".", archivo$dolares_m2total)  # Cambiar comas a puntos (decimales)
  archivo$dolares_m2total <- as.numeric(archivo$dolares_m2total)



# Localidades de Tres de Febrero
localidades <- unique(archivo$nombre)
fechas_formateadas <- unique(archivo$fecha)
zonas_agrupadas <- unique(archivo$zona_agrupada)
ambientes <- unique(archivo$Ambientes)
tipo_de_propiedad <-  unique(archivo$Tipo_propiedad)

zonas_agrupadas_ripte <- unique(na.omit(archivo2$Zonas))
fechas_ripte <- unique(na.omit(archivo2$Fecha))

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





