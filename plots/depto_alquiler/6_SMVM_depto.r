filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}

# Gráfico del SMVM
output$plot_SMVM <- renderPlot({
    # Calcular ratio para "Tres de Febrero" considerando todos los datos (sin distinguir zona)
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(fecha %in% input$fecha_SMVM, Ambientes == "2 ambientes") %>%
        left_join(smvm, by = "fecha") %>%
        mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos)),
               ratio_smvm = promedio / precio_pesos) %>%
        group_by(fecha) %>%
        summarise(Zona_Agrupada = "Tres de Febrero",  # Asignar "Tres de Febrero" como zona
                  ratio_smvm = median(ratio_smvm, na.rm = TRUE), .groups = "drop")
    
    # Crear el gráfico
    ggplot(tres_de_febrero, aes(x = ratio_smvm, y = reorder(fecha, desc(fecha)), fill = "Incidencia del costo sobre el salario SMVM") ) +
        geom_col() +
        geom_text(aes(label = scales::percent(ratio_smvm, accuracy = 0.1)), hjust = -0.2, size = 4) +
        scale_x_continuous(labels = scales::label_percent(accuracy = 1)) + 
        labs(title = "",
             x = "Porcentaje de incidencia sobre el salario (SMVM)",
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

# Tabla dinámica del SMVM
output$table_SMVM <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_SMVM,
                fecha %in% input$fecha_SMVM,
                Ambientes == "2 ambientes") %>%
    left_join(smvm, by = "fecha") %>% # Ahora ambas tablas tienen la columna 'mes'
    group_by(fecha) %>%
    summarise(promedio = mean(promedio, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = promedio, values_fill = 0) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))
  

  reactable(datos_filtrados)
})