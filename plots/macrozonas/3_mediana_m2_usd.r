#Mediana de valor del m2 en venta
output$plot_MDP_mediana_m2 <- renderPlot({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_operacion("DP", "Venta") %>%
    filter(fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(Propiedad = "Tres de Febrero")  # Asignar etiqueta de grupo
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_mediana_m2,
           fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(Propiedad, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)
  
  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = dolar_m2t, color = Propiedad, group = Propiedad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = scales::label_dollar(accuracy = 1)(dolar_m2t)), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
    labs(title = "",
         x = "Fecha",
         y = "Mediana de valor (US$)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica de la mediana de valor del m2 en venta
output$table_MDP_mediana_m2 <- renderReactable({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_operacion("DP", "Venta") %>%
    filter(fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(Propiedad = "Tres de Febrero")
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_mediana_m2,
           fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha, Propiedad) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = dolar_m2t, values_fill = list(dolar_m2t = 0)) %>%
    mutate(across(where(is.numeric), ~ scales::label_dollar(accuracy = 1)(.)))
  
  # Crear la tabla reactable
  reactable(datos_combinados)
})
