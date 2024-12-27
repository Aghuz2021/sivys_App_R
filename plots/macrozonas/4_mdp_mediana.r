# Mediana de valor en departamentos de alquiler

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

output$plot_MDP_mediana <- renderPlot({
  # Calcular la mediana general de "Tres de Febrero" (sin distinguir por ambientes)
  tres_de_febrero <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    mutate(Ambientes = "Tres de Febrero",
           precio_pesos_label = scales::label_dollar()(precio_pesos))

  # Calcular la mediana por ambientes
  datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha, Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    mutate(precio_pesos_label = scales::label_dollar()(precio_pesos))

  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = precio_pesos, color = Ambientes, group = Ambientes)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = precio_pesos_label), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(title = "",
         x = "Fecha",
         y = "Mediana de valor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

#Tabla de la mediana de las publicaciones de departamento en alquiler
output$table_MDP_mediana <- renderReactable({
  # Calcular la mediana general de "Tres de Febrero" (sin distinguir por ambientes)
  tres_de_febrero <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    mutate(Ambientes = "Tres de Febrero")

  # Calcular la mediana por ambientes
  datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha, Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop")

  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = precio_pesos, values_fill = list(precio_pesos = 0)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))

  # Crear la tabla reactable
  reactable(datos_combinados)
})