# Función para filtrar según tipo de propiedad y operación
# Cargar el script de gráficos
output$mapa <- renderLeaflet({
  # Crear el mapa base con las localidades
  leaflet() %>%
    addTiles() %>%  # Mapa base
    addPolygons(
      data = tres_de_febrero,
      color = "#0f6096",  # Color único para todos los bordes
      fillColor = "#3498db85",  # Color único para todos los rellenos
      weight = 1,  # Grosor de los bordes
      opacity = 1,  # Opacidad de los bordes
      fillOpacity = 0.7,  # Opacidad del relleno
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FF5733",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste("Barrio: ", nombre),  # Etiqueta con el nombre de la localidad
      layerId = ~nombre  # Identificador único para cada polígono (localidad)
    ) %>%
    addLegend(
      position = "topright",  # Ubicación de la leyenda (arriba a la derecha)
      colors = rep("#ffffff00", length(unique(tres_de_febrero$nombre))),  # Un color repetido para cada localidad
      labels = unique(tres_de_febrero$nombre),  # Nombres de todas las localidades
      title = "Localidades",  # Título de la leyenda
      opacity = 1  # Opacidad de la leyenda
    )
})
output$macrozonas <- renderLeaflet({
  # Filtrar los datos para mostrar solo la macrozona "Desarrollo Prioritario"
  macrozona_filtrada <- macrozonas %>%
    filter(macrozona == "Desarrollo Prioritario")

  # Crear el mapa base con las localidades filtradas
  leaflet() %>%
    addTiles() %>%  # Mapa base
    addPolygons(
      data = macrozona_filtrada,
      color = "#6e0372",  # Color único para todos los bordes
      fillColor = "#ec17f385",  # Color único para todos los rellenos
      weight = 1,  # Grosor de los bordes
      opacity = 1,  # Opacidad de los bordes
      fillOpacity = 0.7,  # Opacidad del relleno
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FF5733",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste(macroz_des)  # Etiqueta con la descripción de la macrozona
    ) %>%
    
    # Agregar el borde de Tres de Febrero
    addPolygons(
      data = tres_de_febrero,
      color = "#2E86C1",  # Color de los bordes de Tres de Febrero
      fillColor = NA,  # Sin relleno para Tres de Febrero (solo borde)
      weight = 2,  # Grosor de los bordes
      opacity = 1
    )%>%
    addLegend(
      position = "topright",  # Ubicación de la leyenda (arriba a la derecha)
      colors = "#6e0372",  # Color violeta para la leyenda
      labels = "Macrozonas de Desarrollo Prioritario",  # Etiqueta en la leyenda
      opacity = 1  # Opacidad de la leyenda
    )
})



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


# Gráfico de Casas en Venta
output$plot_casa <- renderPlot({
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_casa, 
           fecha %in% input$fecha_casa) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Casas en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Venta de Casas
output$table_casa <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_casa, 
    fecha %in% input$fecha_casa) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})

# Gráfico de m² Publicados
output$plot_m2 <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter( nombre %in% input$localidad_m2, fecha %in% input$fecha_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "Cantidad Total de m² Publicados de Casas en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica para m² Publicados
output$table_m2 <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter( nombre %in% input$localidad_m2, fecha %in% input$fecha_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})

# Gráfico de Línea de Mediana de Precio en USD de Casas en Venta
output$plot_mediana_usd_por_localidad <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta", 
           nombre %in% input$localidad_mediana_usd, 
           fecha %in% input$fecha_mediana_usd) %>%
    group_by(nombre, fecha) %>%
    summarise(
      Tipo_propiedad = n(),  # Contar publicaciones
      precio_dolares = if_else( Tipo_propiedad > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados
  
  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = precio_dolares, color = nombre, group = nombre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_dolares, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio en USD de Casas en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

output$table_usd_por_localidad <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter(nombre %in% input$localidad_mediana_usd, 
               fecha %in% input$fecha_mediana_usd) %>%
        group_by(fecha, nombre) %>%
        summarise(
            precio_dolares = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
            .groups = "drop"
        ) %>%
        filter(!is.na(precio_dolares)) %>%
        pivot_wider(names_from = fecha, values_from = precio_dolares, values_fill = 0)
    
    reactable(datos_filtrados)
})

output$plot_mediana_usd_por_za <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(
      Tipo_propiedad == "Casa", 
      Tipo_operacion == "Venta", 
      zona_agrupada %in% input$zona_agrupada_casa, 
      fecha %in% input$fecha_zona_agrupada_Casa
    ) %>%
    group_by(zona_agrupada, fecha) %>%
    summarise(
      cantidad_propiedades = n(),  # Contar publicaciones
      precio_dolares = if_else(cantidad_propiedades > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados

  if (nrow(datos_filtrados) == 0) {
    return(NULL)  # No generar gráfico si no hay datos
  }

  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = precio_dolares, color = zona_agrupada, group = zona_agrupada)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_dolares, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio en USD de Casas en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

output$table_mediana_usd_za_casa <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter(zona_agrupada %in% input$zona_agrupada_casa, 
               fecha %in% input$fecha_zona_agrupada_Casa) %>%
        group_by(zona_agrupada, fecha) %>%
        summarise(
            precio_dolares = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
            .groups = "drop"
        ) %>%
        filter(!is.na(precio_dolares)) %>%
        pivot_wider(names_from = fecha, values_from = precio_dolares, values_fill = 0)
    
    reactable(datos_filtrados)
})

# ---Mediana de valor en usd del m² por ZA.
output$plot_mediana_usd_por_m2_za <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(
      Tipo_propiedad == "Casa", 
      Tipo_operacion == "Venta", 
      zona_agrupada %in% input$za_mediana_usd_m2, 
      fecha %in% input$fecha_mediana_usd_m2
    ) %>%
    group_by(zona_agrupada, fecha) %>%
    summarise(
      cantidad_propiedades = n(),  # Contar publicaciones
      dolares_m2total = if_else(cantidad_propiedades > 20, median(dolares_m2total, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(dolares_m2total))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados

  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = dolares_m2total, color = zona_agrupada, group = zona_agrupada)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(dolares_m2total, 2)), vjust = -0.5, size = 4) +
    labs(title = "",
         x = "Fecha",
         y = "Mediana de m2 (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})





output$plot_mediana_usd_por_m2_localidad <- renderPlot({
  # Convertir la columna dolares_m2total a numérica asegurando el formato correcto

  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(
      Tipo_propiedad == "Casa", 
      Tipo_operacion == "Venta", 
      nombre %in% input$m2_localidad_casa, 
      fecha %in% input$fecha_m2_localidad_casa
    ) %>%
    group_by(nombre, fecha) %>%
    summarise(
      cantidad_propiedades = n(),  # Contar publicaciones
      dolares_m2total = median(dolares_m2total, na.rm = TRUE),  # Calcular la mediana directamente
      .groups = "drop"
    ) %>%
    filter(!is.na(dolares_m2total))  # Filtrar filas donde se calculó la mediana

  # Verificar el contenido de los datos filtrados
  print(datos_filtrados)

  # Si no hay datos, retornar NULL para no generar el gráfico
  if (nrow(datos_filtrados) == 0) {
    return(NULL)
  }

  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = dolares_m2total, color = nombre, group = nombre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(dolares_m2total, 2)), vjust = -0.5, size = 4) +
    labs(
      title = "Mediana de Precio en USD por m² de Casas en Venta por Localidad",
      x = "Fecha",
      y = "Mediana en USD por m²"
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


















output$plot_ripte_necesario <- renderPlot({
  # Filtrar los datos según las fechas y propiedades seleccionadas
  datos_filtrados <- archivo2 %>%
    filter(
      tipo_propiedad == "Casa", 
      Fecha %in% input$fechas_ripte  # Filtrar por fechas seleccionadas
    ) %>%
    group_by(Fecha)
    

  # Verifica el contenido de datos_filtrados
  print(datos_filtrados)

  # Generar el gráfico para visualizar el Ripte necesario
  ggplot(datos_filtrados, aes(x = fecha, y = Ripte_necesario_promedio)) +
    geom_line(color = "blue") +
    labs(
      title = "Promedio de Ripte Necesario por Fecha",
      x = "Fecha",
      y = "Ripte Necesario"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Mejorar la visualización de las fechas
})


#-------depto------#
# Gráfico de Casas en Venta
output$plot_depto <- renderPlot({
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Departamento", Tipo_operacion == "Venta", 
           nombre %in% input$localidad_depto, fecha %in% input$fecha_depto) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de departamento en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

output$plot_m2_departamento <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Venta") %>%
        filter( nombre %in% input$localidad_m2_departamento, fecha %in% input$fecha_m2_departamento)%>%
    group_by(fecha, nombre) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "Cantidad Total de m² Publicados de Departamentos en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})
# Tabla dinámica para m² Publicados
output$table_m2_departamento<- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Venta") %>%
        filter( nombre %in% input$localidad_m2_departamento, fecha %in% input$fecha_m2_departamento)%>%
    group_by(fecha, nombre) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})

# Gráfico de Línea de Mediana de Precio en USD de Casas en Venta
output$plot_mediana_departamento <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Departamento", Tipo_operacion == "Venta", 
           nombre %in% input$localidad_mediana_departamento, 
           fecha %in% input$fecha_mediana_departamento) %>%
    group_by(nombre, fecha) %>%
    summarise(
      Tipo_propiedad = n(),  # Contar publicaciones
      precio_dolares = if_else( Tipo_propiedad > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados
  
  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = precio_dolares, color = nombre, group = nombre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_dolares, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio en USD de departamento en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

output$table_mediana_localidad_departamento <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Venta") %>%
        filter(nombre %in% input$localidad_mediana_departamento, 
               fecha %in% input$fecha_mediana_departamento) %>%
        group_by(fecha, nombre) %>%
        summarise(
            precio_dolares = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
            .groups = "drop"
        ) %>%
        filter(!is.na(precio_dolares)) %>%
        pivot_wider(names_from = fecha, values_from = precio_dolares, values_fill = 0)
        
    reactable(datos_filtrados)
})

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
