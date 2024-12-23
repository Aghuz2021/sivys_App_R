# Cargar librerías
source("Librerias/librerias.r")

archivo <- read.csv("C://Users//Agustin//Documents//sivys_App_R//bd//sivys_bd.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
archivo2 <- read.csv("C://Users//Agustin//Documents//sivys_App_R//bd//ripte(data).csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
geojson_path <- "C://Users//Agustin//Documents//sivys_App_R//bd//zonas.geojson" # Cambiar por tu archivo
geojson_macrozonas <- "C://Users//Agustin//Documents//sivys_App_R//bd//macrozonas.geojson" # Cambiar por tu archivo


tres_de_febrero <- st_read(geojson_path)
print(tres_de_febrero)
macrozonas <- st_read(geojson_macrozonas)

# Transformar la columna dolares_m2total a entero
archivo$dolares_m2total <- as.integer(gsub("[,.]", "", archivo$dolares_m2total))




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
  source("plots/home/plot_mapas_home.R", local = TRUE)
  source("plots/casa/cant_casas.R", local = TRUE)
  source("plots/casa/cant_m2.R", local = TRUE)
  source("plots/casa/mediana_usd.R", local = TRUE)
  source("plots/casa/mediana_usd_za.R", local = TRUE)
  source("plots/casa/mediana_usd_m2_za.R", local = TRUE)
  source("plots/plot_venta_Casa.R", local = TRUE)

}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)








