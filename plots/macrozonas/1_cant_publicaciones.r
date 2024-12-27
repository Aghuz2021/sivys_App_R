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




output$plot_MDP_pub <- renderPlot({
  # Calcular el total de publicaciones para "Tres de Febrero" (sin distinguir propiedad)
  tres_de_febrero <- filtro_operacion("DP", "Venta") %>%
    filter(fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha) %>%
    summarise(Propiedad = "Tres de Febrero",  # Asignar el nombre "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")
  
  # Calcular el conteo de publicaciones para las propiedades seleccionadas
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_pub, 
           fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Propiedad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Combinar los datos de "Tres de Febrero" con los datos filtrados
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    mutate(Propiedad = factor(Propiedad, levels = c("Tres de Febrero", unique(datos_filtrados$Propiedad))))
  
  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = conteo_publicaciones, color = Propiedad, group = Propiedad)) +
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

# Tabla dinámica para Cantidad de Publicaciones de Venta
output$table_MDP_pub <- renderReactable({
  # Calcular el total de publicaciones para "Tres de Febrero" (sin distinguir propiedad)
  tres_de_febrero <- filtro_operacion("DP", "Venta") %>%
    filter(fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha) %>%
    summarise(Propiedad = "Tres de Febrero",  # Asignar el nombre "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")
  
  # Calcular el conteo de publicaciones para las propiedades seleccionadas
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_pub, 
           fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Propiedad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Combinar los datos de "Tres de Febrero" con los datos filtrados
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
  
  # Crear la tabla reactable
  reactable(datos_combinados)
})
