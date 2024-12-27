



#  output$mapa <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%  # Mapa base
#       addPolygons(
#         data = tres_de_febrero,
#         color = "#2E86C1",  # Color de los bordes
#         fillColor = "#AED6F1",  # Color de relleno
#         weight = 1,  # Grosor de los bordes
#         opacity = 1,  # Opacidad de los bordes
#         fillOpacity = 0.7,  # Opacidad del relleno
#         highlightOptions = highlightOptions(
#           weight = 3,
#           color = "#FF5733",
#           fillOpacity = 0.9,
#           bringToFront = TRUE
#         ),
#         label = ~paste("Barrio: ", nombre)  # Cambia "nombre" por el atributo relevante
#       )
#   })



filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}

















#-------depto------#
# Gráfico de Casas en Venta



# Gráfico de Línea de Mediana de Precio en USD de Casas en Venta


output$cantidad_publicaciones_ambiente_za_depto <- renderPlot({
  # Filtrar y agrupar los datos
  datos_filtrados <- archivo %>%
    filter(
      Tipo_propiedad == "Departamento", 
      Tipo_operacion == "Venta", 
      zonas_agrupadas %in% input$depto_ambientes_za, 
      fecha %in% input$depto_ambientes_fecha, 
      ambientes %in% input$depto_ambientes
    ) %>%
    group_by(fecha, ambientes, zonas_agrupadas) %>%
    summarise(
      mediana_valor = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_), # Calcular mediana si hay más de 20 publicaciones
      .groups = "drop"
    )
  
  # Generar el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = mediana_valor, color = zonas_agrupadas, group = zonas_agrupadas)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = round(mediana_valor, 2)), vjust = -0.5, size = 4) +
    labs(
      title = "Mediana de Valor de Departamentos en Venta por Fecha y Zona",
      x = "Fecha",
      y = "Mediana del Valor (USD)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
})
