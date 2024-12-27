filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}

output$plot_mediana_ZA <- renderPlot({
    # Calcular los totales por mes y por ambiente para "Tres de Febrero"
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter(fecha %in% input$fecha_mediana_ZA,
               Ambientes %in% input$ambientes_mediana_ZA) %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
        group_by(fecha, Ambientes) %>%
        filter(n() >= 20) %>%
        summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
        mutate(precio_pesos_label = scales::label_dollar()(precio_pesos),
               Zona_Agrupada = "Tres de Febrero")

    # Calcular la mediana de valor por Zona Agrupada y Ambientes
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_mediana_ZA,
               fecha %in% input$fecha_mediana_ZA,
               Ambientes %in% input$ambientes_mediana_ZA) %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
        group_by(fecha, Zona_Agrupada, Ambientes) %>%
        filter(n() >= 20) %>%
        summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
        mutate(precio_pesos_label = scales::label_dollar()(precio_pesos))

    # Combinar los datos
    datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

    # Si no hay datos, no dibujar nada
    if (nrow(datos_combinados) == 0) {
        return(NULL)
    }

    # Crear el gráfico
    ggplot(datos_combinados, aes(x = fecha, y = precio_pesos, color = interaction(Zona_Agrupada, Ambientes), group = interaction(Zona_Agrupada, Ambientes))) +
        geom_line() +
        geom_point(size = 2) +
        geom_text(aes(label = precio_pesos_label), vjust = -0.5, size = 4) +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(title = "",
             x = "Fecha",
             y = "Mediana de valor",
             color = "Zona y Ambientes") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9))
})

# Tabla dinámica de la mediana de valor de las publicaciones por ZA
output$table_mediana_ZA <- renderReactable({
    # Calcular los totales por mes y por ambiente para "Tres de Febrero"
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter(fecha %in% input$fecha_mediana_ZA,
               Ambientes %in% input$ambientes_mediana_ZA) %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
        group_by(fecha, Ambientes) %>%
        filter(n() >= 20) %>%
        summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
        mutate(Zona_Agrupada = "Tres de Febrero")

    # Calcular la mediana de valor por Zona Agrupada y Ambientes
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_mediana_ZA,
               fecha %in% input$fecha_mediana_ZA,
               Ambientes %in% input$ambientes_mediana_ZA) %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
        group_by(fecha, Zona_Agrupada, Ambientes) %>%
        filter(n() >= 20) %>%
        summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop")

    # Combinar los datos
    datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
        pivot_wider(names_from = Ambientes, values_from = precio_pesos, values_fill = list(precio_pesos = NA)) %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))

    # Si no hay datos, no mostrar nada
    if (nrow(datos_combinados) == 0) {
        return(NULL)
    }

    # Crear la tabla reactable
    reactable(datos_combinados)
})