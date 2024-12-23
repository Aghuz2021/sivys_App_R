# --- Mediana de valor en USD del m² por ZA ---


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
      conteo = n(),  # Contar publicaciones
      dolares_m2total = median(dolares_m2total, na.rm = TRUE), NA_real_, 
      
    ) %>%
    filter(!is.na(dolares_m2total))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verificar el contenido de datos_filtrados

  # Si no hay datos después de filtrar, no generar el gráfico
  if (nrow(datos_filtrados) == 0) {
    return(NULL)
  }

  # Crear el gráfico de línea de la mediana de precio en dólares por m2 por zona agrupada
  ggplot(datos_filtrados, aes(x = fecha, y = dolares_m2total, color = zona_agrupada, group = zona_agrupada)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(dolares_m2total, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio por m² (USD) de Casas en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio por m² (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})
