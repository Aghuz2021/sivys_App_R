filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}
#Gráfico de la cantidad de m2
output$plot_alq_m2 <- renderPlot({
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_m2) %>%  # Filtrar según las fechas seleccionadas
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la línea "Tres de Febrero"
              total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
            
  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_m2, 
           fecha %in% input$fecha_depto_m2) %>%
    group_by(fecha, Localidad) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

  datos_combinados <- datos_combinados %>%
    mutate(Localidad = factor(Localidad, levels = c("Tres de Febrero", unique(datos_filtrados$Localidad))))
  
  ggplot(datos_combinados, aes(x = fecha, y = total_area, color = Localidad, group = Localidad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica para m² Publicados
output$table_alq_m2 <- renderReactable({
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_m2) %>%  # Filtrar según las fechas seleccionadas
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la fila "Tres de Febrero"
              Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_m2, 
           fecha %in% input$fecha_depto_m2) %>%
    group_by(fecha, Localidad) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_combinados)
})