# Cargar librerías
source("Librerias/librerias.r")

archivo <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//sivys_bd.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
archivo2 <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//ripte(data).csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
geojson_path <- "C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//zonas.geojson" # Cambiar por tu archivo
geojson_macrozonas <- "C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//macrozonas.geojson" # Cambiar por tu archivo

ripte <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//RIPTE.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
smvm <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//SMVM.csv", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

zonas_agrupadas_geo <- read.csv("C://Users//Usuario//Documents//GitHub//sivys_App_R//bd//zonas_agrupadas.geojson", sep = ";", fileEncoding = "UTF-8", stringsAsFactors = FALSE)





 # Crear datos de ejemplo para el gráfico
  datos <- data.frame(
    Fecha = c("2023-11", "2023-12", "2024-01", "2024-02", "2024-03", "2024-04", 
              "2024-05", "2024-06", "2024-07", "2024-08", "2024-09"),
    ripte = c(447080, 484298, 555269, 619007, 705833, 819502, 879483, 933180, 
              994681, 1032410, 1075145),
    Ripte_necesario = c(128, 131, 134, 119, 92, 80, 86, 89, 87, 79, 71),
    tipo_propiedad = rep("Casa", 11)
  )

  # Crear el data frame
smvm_casa <- data.frame(
  Fecha = c("2023-11", "2023-12", "2024-01", "2024-02", "2024-03", 
            "2024-04", "2024-05", "2024-06", "2024-07", "2024-08", "2024-09"),
  smvm = c(146000, 156000, 156000, 180000, 202800, 221052, 234315, 
           234315, 254231, 262433, 268056),
  smvm_necesario = c(391, 408, 478, 410, 321, 296, 325, 354, 340, 311, 286),
  tipo_propiedad = rep("Casa", 11)
)

# Crear el data frame
smvm_depto <- data.frame(
  Fecha = c("2023-11", "2023-12", "2024-01", "2024-02", "2024-03", 
            "2024-04", "2024-05", "2024-06", "2024-07", "2024-08", "2024-09"),
  smvm = c(146000, 156000, 156000, 180000, 202800, 221052, 234315, 
           234315, 254231, 262433, 268056),
  smvm_necesario = c(358, 367, 428, 368, 296, 276, 297, 321, 310, 289, 270)
)

# Ver el data frame
print(datos)






tres_de_febrero <- st_read(geojson_path)
print(tres_de_febrero)
macrozonas <- st_read(geojson_macrozonas)
zonas_agrupadas_geojson <- st_read(zonas_agrupadas_geo)
# Transformar la columna dolares_m2total a entero
archivo$dolares_m2total <- as.integer(gsub("[,.]", "", archivo$dolares_m2total))




datos_Fechas <- unique(datos$Fecha)
smvm_fechas <- unique(smvm_casa$Fecha)
# Localidades de Tres de Febrero
localidades <- unique(archivo$nombre)
fechas_formateadas <- unique(archivo$fecha)

zonas_agrupadas <- unique(archivo$zona_agrupada)
ambientes <- unique(archivo$Ambientes)
tipo_de_propiedad <-  unique(archivo$Tipo_propiedad)

zonas_agrupadas_ripte <- unique(na.omit(archivo2$Zonas))
fechas_ripte <- unique(na.omit(archivo2$Fecha))

ripte$promedio <- as.numeric(gsub("[^0-9.]", "", ripte$promedio))
smvm$promedio <- as.numeric(gsub("[^0-9.]", "", smvm$promedio))

options(scipen = 999)


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

 
  # Llamamos a los graficos de cada categoria.
  source("plots/inicio/plot_mapas_home.R", local = TRUE)
  # llamamos a casa

  source("plots/casa/1_cant_casas.R", local = TRUE)
  source("plots/casa/2_cant_m2.R", local = TRUE)
  source("plots/casa/3_mediana_usd_localidad.R", local = TRUE)
  source("plots/casa/4_mediana_usd_za.R", local = TRUE)
  source("plots/casa/5_mediana_usd_m2_za.R", local = TRUE)
  source("plots/casa/6_mediana_usd_por_m2_localidad.R", local = TRUE)
  source("plots/casa/7_ripte.R", local = TRUE)
  source("plots/casa/8_smvm.R", local = TRUE)

  source("plots/depto/1_cant_depto.r", local = TRUE)
  source("plots/depto/3_cant_m2.r", local = TRUE)
  source("plots/depto/4_mediana_usd_localidad.R", local = TRUE)
  source("plots/depto/5_mediana_usd_za.R", local = TRUE)


  #alquiler
  source("plots/depto_alquiler/1_cant_publicaciones.R", local = TRUE)
  source("plots/depto_alquiler/2_Cantidad_m2.R", local = TRUE)
  source("plots/depto_alquiler/3_cant_m2_za.R", local = TRUE)
  source("plots/depto_alquiler/4_mediana_usd_za.R", local = TRUE)
  source("plots/depto_alquiler/5_ripte_depto.R", local = TRUE)
  source("plots/depto_alquiler/6_SMVM_depto.R", local = TRUE)


  #Macrozona
  source("plots/macrozonas/1_cant_publicaciones.R", local = TRUE)
  source("plots/macrozonas/2_cant_publicaciones_depto.R", local = TRUE)
  source("plots/macrozonas/3_mediana_m2_usd.R", local = TRUE)
  source("plots/macrozonas/4_mdp_mediana.R", local = TRUE)

  source("plots/plot_venta_Casa.R", local = TRUE)

}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


