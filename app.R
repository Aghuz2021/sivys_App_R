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

archivo <- read.csv("C://Users//Usuario//Documents//vf//sivys_App_R//bd//sivys_bd.csv",fileEncoding = "UTF-8", stringsAsFactors = FALSE)
salario_minimo <-  read.csv("C://Users//Usuario//Documents//vf//sivys_App_R//bd//SMVM.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
ripte <- read.csv("C://Users//Usuario//Documents//vf//sivys_App_R//bd//RIPTE.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
smvm <- read.csv("C://Users//Usuario//Documents//vf//sivys_App_R//bd//SMVM.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

archivo$dolares <- as.numeric(archivo$precio_dolares)
ripte$promedio <- as.numeric(gsub("[^0-9.]", "", ripte$promedio))
smvm$promedio <- as.numeric(gsub("[^0-9.]", "", smvm$promedio))

options(scipen = 999)

# datos_filtrados <- archivo %>% 
#   filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta") %>% 
#   group_by(nombre, fecha) %>%  # Agrupar por 'nombre' y 'fecha'
#   summarise(
#     precio_dolares = median(precio_dolares, na.rm = TRUE),  # Calcular mediana si hay más de 20 publicaciones
#     .groups = "drop"
#   )


# Localidades de Tres de Febrero
localidades <- unique(archivo$nombre)
fechas_formateadas <- unique(archivo$fecha)
zonas_agrupadas <- unique(archivo$zona_agrupada)
ambientes <- unique(archivo$Ambientes)
tipo_de_propiedad <-  unique(archivo$Tipo_propiedad)



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



print(names(ripte))
str(archivo)
str(ripte)
