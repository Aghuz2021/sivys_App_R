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


