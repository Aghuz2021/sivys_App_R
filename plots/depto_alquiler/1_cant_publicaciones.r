filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}


output$plot_alq <- renderPlot({
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_alqui) %>%
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la localidad "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")

    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_alqui, 
           fecha %in% input$fecha_depto_alqui) %>%
    group_by(fecha, Localidad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

  datos_combinados <- datos_combinados %>%
    mutate(Localidad = factor(Localidad, levels = c("Tres de Febrero", unique(datos_filtrados$Localidad))))
  
  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = conteo_publicaciones, color = Localidad, group = Localidad)) +
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

# Tabla dinámica de Cantidad de Publicaciones
output$table_alq <- renderReactable({
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_alqui) %>%
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la fila "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")

  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_alqui, 
           fecha %in% input$fecha_depto_alqui) %>%
    group_by(fecha, Localidad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)

    reactable(datos_combinados)
})