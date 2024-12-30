# Gráfico de salarios necesarios para comprar una casa de 100 m²
output$plot_salarios_casa <- renderPlot({
  
  # Filtrar los datos según las fechas seleccionadas
  datos_filtrados <- datos %>%
    filter(Fecha %in% input$fecha_ripte_venta)  # Filtrar por las fechas seleccionadas
  
  # Verificar si hay datos después del filtrado
  if (nrow(datos_filtrados) == 0) {
    return(NULL)
  }

 # Crear el gráfico de barras para Ripte necesario
ggplot(datos_filtrados, aes(x = Fecha, y = Ripte_necesario)) +
  geom_col(fill = "blue", color = "black", width = 0.7) +  # Gráfico de barras
  geom_text(aes(label = Ripte_necesario), vjust = -0.5, size = 4) +  # Etiquetas sobre las barras
  labs(
    title = "Ripte Necesario para Comprar una Casa de 100 m²",
    x = "Fecha",
    y = "Ripte Necesario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )
})

# Tabla dinámica del RIPTE
output$table_RIPTE_Casa <- renderReactable({
   
})
