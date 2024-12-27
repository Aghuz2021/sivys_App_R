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