filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}
# Gráfico del RIPTE
output$plot_RIPTE <- renderPlot({
    # Calcular ratio para "Tres de Febrero"
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(fecha %in% input$fecha_RIPTE, Ambientes == "2 ambientes") %>%
        left_join(ripte, by = "fecha") %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos)),
               ratio = precio_pesos / promedio) %>%
        group_by(fecha) %>%
        summarise(Zona_Agrupada = "Tres de Febrero",  # Crear etiqueta "Tres de Febrero"
                  ratio = median(ratio, na.rm = TRUE), .groups = "drop") 

    # Crear gráfico de barras horizontales
    ggplot(tres_de_febrero, aes(x = ratio, y = reorder(fecha, desc(fecha)), fill = "Incidencia del costo sobre el salario RIPTE")) +
        geom_col() +
        geom_text(aes(label = scales::percent(ratio, accuracy = 0.1)), hjust = -0.2, size = 4) +
        scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(title = "",
             x = "Porcentaje de incidencia sobre el salario (RIPTE)",
             y = "Meses",
             fill = "") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.position = "top",
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9))
})

# Tabla dinámica del RIPTE
output$table_RIPTE <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_RIPTE,
                fecha %in% input$fecha_RIPTE,
                Ambientes == "2 ambientes") %>%
    left_join(ripte, by = "fecha") %>% # Ahora ambas tablas tienen la columna 'mes'
    group_by(fecha) %>%
    summarise(promedio = mean(promedio, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = promedio, values_fill = 0) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))
  

  reactable(datos_filtrados)
})
