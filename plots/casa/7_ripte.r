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