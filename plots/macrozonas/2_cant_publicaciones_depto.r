#Cantidad de publicaciones de departamentos en alquiler


# Cantidad de publicaciones de venta
filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}

# Función para filtrar según si es Desarrollo Prioritario y tipo de operación
filtro_operacion <- function(desarrollo.prioritario, Tipo_operacion) {
  archivo %>%
    filter(desarrollo.prioritario == !!desarrollo.prioritario,
           Tipo_operacion == !!Tipo_operacion)
}

# Función para filtrar según tipo de propiedad, operación y si es Desarrollo Prioritario
filtro_propiedad_operacion_DP <- function(Tipo_propiedad, Tipo_operacion, desarrollo.prioritario) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion,
           desarrollo.prioritario == !!desarrollo.prioritario)
}


output$plot_MDP_depto_alq <- renderPlot({
  datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
    filter(fecha %in% input$fecha_MDP_depto_alq) %>%
    group_by(fecha) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = fecha, group = 1)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Cantidad de Publicaciones de Departamentos en Alquiler
output$table_MDP_depto_alq <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(fecha %in% input$fecha_MDP_depto_alq) %>%
    group_by(fecha) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})
