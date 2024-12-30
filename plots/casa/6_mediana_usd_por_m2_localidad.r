filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(
      Tipo_propiedad == !!Tipo_propiedad, 
      Tipo_operacion == !!Tipo_operacion
    )
}

output$plot_mediana_usd_por_m2_localidad <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(
      nombre %in% input$m2_localidad_casa,
      fecha %in% input$fecha_m2_localidad_casa
    ) %>%
    group_by(nombre, fecha) %>%
    summarise(
      cantidad_propiedades = n(),  # Contar publicaciones
      dolares_m2total = median(dolares_m2total, na.rm = TRUE),  # Calcular la mediana
      .groups = "drop"
    ) %>%
    filter(!is.na(dolares_m2total))  # Filtrar filas sin datos válidos

  # Si no hay datos, no dibujar nada
  if (nrow(datos_filtrados) == 0) {
    return(NULL)
  }

  # Crear el gráfico de línea de la mediana de precio en dólares por m²
  ggplot(datos_filtrados, aes(x = fecha, y = dolares_m2total, color = nombre, group = nombre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(dolares_m2total, 2)), vjust = -0.5, size = 4) +
    labs(
      title = "Mediana de Precio en USD por m² de Casas en Venta por Localidad",
      x = "Fecha",
      y = "Mediana en USD por m²",
      color = "Localidad"
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


