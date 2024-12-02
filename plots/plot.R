# Función para filtrar según tipo de propiedad y operación
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
output$plot_mediana_usd_por_za <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta", 
           zona_agrupada %in% input$localidad_mediana_usd_za, 
           fecha %in% input$fecha_mediana_usd_za) %>%
    group_by(zona_agrupada, fecha) %>%
    summarise(
      Tipo_propiedad = n(),  # Contar publicaciones
      precio_dolares = if_else( Tipo_propiedad > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados
  
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

