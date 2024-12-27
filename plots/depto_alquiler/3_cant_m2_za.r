filtro_propiedad_operacion <- function(Tipo_propiedad, Tipo_operacion) {
  archivo %>%
    filter(Tipo_propiedad == !!Tipo_propiedad, 
           Tipo_operacion == !!Tipo_operacion)
}

output$plot_alq_m2_ZA <- renderPlot({
    # Calcular los m² totales publicados de "Tres de Febrero" (sin distinguir por Zona Agrupada)
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter(fecha %in% input$fecha_depto_m2_ZA) %>%
        group_by(fecha) %>%
        summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
        mutate(Zona_Agrupada = "Tres de Febrero")
    
    # Calcular los m² por Zona Agrupada
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_depto_m2_ZA,
               fecha %in% input$fecha_depto_m2_ZA) %>%
        group_by(fecha, Zona_Agrupada) %>%
        summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
    
    # Combinar los datos
    datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

    # Crear el gráfico
    ggplot(datos_combinados, aes(x = fecha, y = total_area, color = Zona_Agrupada, group = Zona_Agrupada)) +
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

# Tabla dinámica de la cantidad de m2 por Zona Agrupada
output$table_alq_m2_ZA <- renderReactable({
    # Calcular los m² totales publicados de "Tres de Febrero" (sin distinguir por Zona Agrupada)
    tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter(fecha %in% input$fecha_depto_m2_ZA) %>%
        group_by(fecha) %>%
        summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
        mutate(Zona_Agrupada = "Tres de Febrero")
    
    # Calcular los m² por Zona Agrupada
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_depto_m2_ZA,
               fecha %in% input$fecha_depto_m2_ZA) %>%
        group_by(fecha, Zona_Agrupada) %>%
        summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
    
    # Combinar los datos
    datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
        pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
    
    # Crear la tabla reactable
    reactable(datos_combinados)
})