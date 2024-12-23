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