# Función para filtrar según tipo de propiedad y operación
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

# Gráfico de Casas en Venta
output$plot_casa <- renderPlot({
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_casa, 
           fecha %in% input$fecha_casa) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Casas en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Venta de Casas
output$table_casa <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_casa, 
    fecha %in% input$fecha_casa) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})

# Gráfico de m² Publicados
output$plot_m2 <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter( nombre %in% input$localidad_m2, fecha %in% input$fecha_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "Cantidad Total de m² Publicados de Casas en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica para m² Publicados
output$table_m2 <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter( nombre %in% input$localidad_m2, fecha %in% input$fecha_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})

# Gráfico de Línea de Mediana de Precio en USD de Casas en Venta
output$plot_mediana_usd_por_localidad <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta", 
           nombre %in% input$localidad_mediana_usd, 
           fecha %in% input$fecha_mediana_usd) %>%
    group_by(nombre, fecha) %>%
    summarise(
      Tipo_propiedad = n(),  # Contar publicaciones
      precio_dolares = if_else( Tipo_propiedad > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados
  
  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = precio_dolares, color = nombre, group = nombre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_dolares, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio en USD de Casas en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})
output$table_usd_por_localidad <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
        filter(nombre %in% input$localidad_mediana_usd, 
               fecha %in% input$fecha_mediana_usd) %>%
        group_by(fecha, nombre) %>%
        summarise(
            precio_dolares = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
            .groups = "drop"
        ) %>%
        filter(!is.na(precio_dolares)) %>%
        pivot_wider(names_from = fecha, values_from = precio_dolares, values_fill = 0)
    
    reactable(datos_filtrados)
})


#-------depto------#
# Gráfico de Casas en Venta
output$plot_depto <- renderPlot({
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Departamento", Tipo_operacion == "Venta", 
           nombre %in% input$localidad_depto, fecha %in% input$fecha_depto) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Casas en Venta por Fecha y Localidad",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})
output$plot_mediana_usd_por_za <- renderPlot({
  # Filtrar los datos según las localidades y fechas seleccionadas
  datos_filtrados <- archivo %>%
    filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta", 
           zona_agrupada %in% input$localidad_mediana_usd_za, 
           fecha %in% input$fecha_mediana_usd_za) %>%
    group_by(zona_agrupada, fecha) %>%
    summarise(
      Tipo_propiedad = n(),  # Contar publicaciones
      precio_dolares = if_else( Tipo_propiedad > 20, median(precio_dolares, na.rm = TRUE), NA_real_), 
      .groups = "drop"
    ) %>%
    filter(!is.na(precio_dolares))  # Filtrar filas donde se calculó la mediana

  print(datos_filtrados)  # Verifica el contenido de datos_filtrados
  
  # Crear el gráfico de línea de la mediana de precio en dólares por localidad
  ggplot(datos_filtrados, aes(x = fecha, y = precio_dolares, color = zona_agrupada, group = zona_agrupada)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_dolares, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de Precio en USD de Casas en Venta por Localidad",
         x = "Fecha",
         y = "Mediana de Precio (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

#---Departamento en alquiler---#

#Gráfico de la cantidad de publicaciones
output$plot_alq <- renderPlot({
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto, 
           fecha %in% input$fecha_depto) %>%
    group_by(fecha, Localidad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = Localidad, group = Localidad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones por Localidad",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica de Cantidad de Publicaciones
output$table_alq <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto, 
    fecha %in% input$fecha_depto) %>%
    group_by(fecha, Localidad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})

#Gráfico de la cantidad de m2
output$plot_alq_m2 <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Localidad = nombre) %>%
        filter( Localidad %in% input$localidad_depto_m2, fecha %in% input$fecha_depto_m2)%>%
    group_by(fecha, Localidad) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = Localidad, group = Localidad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "m² Publicados por Localidad",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica para m² Publicados
output$table_alq_m2 <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Localidad = nombre) %>%
        filter( Localidad %in% input$localidad_depto_m2, fecha %in% input$fecha_depto_m2)%>%
    group_by(fecha, Localidad) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})


#Grafico de la cantidad de m2 por Zona Agrupada
output$plot_alq_m2_ZA <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_depto_m2_ZA,
                fecha %in% input$fecha_depto_m2_ZA) %>%
    group_by(fecha, Zona_Agrupada) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = Zona_Agrupada, group = Zona_Agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "m² de las Publicaciones por ZA",
         x = "Fecha",
         y = "Total de m² Publicados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica de la cantidad de m2 por Zona Agrupada
output$table_alq_m2_ZA <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_depto_m2_ZA, 
                fecha %in% input$fecha_depto_m2_ZA)%>%
    group_by(fecha, Zona_Agrupada) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})


#Grafico de la mediana de valor de las publicaciones por ZA
output$plot_mediana_ZA <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_mediana_ZA,
                fecha %in% input$fecha_mediana_ZA,
                Ambientes %in% input$ambientes_mediana_ZA) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha, Zona_Agrupada, Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    mutate(precio_pesos_label = scales::label_dollar()(precio_pesos))

    if (nrow(datos_filtrados) == 0) {
        return(NULL)  # No dibujar nada si no hay datos
    }
  
  ggplot(datos_filtrados, aes(x = fecha, y = precio_pesos, color = interaction(Zona_Agrupada, Ambientes), group = interaction(Zona_Agrupada, Ambientes))) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = precio_pesos_label), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(title = "Mediana de valor (en pesos ARS) de las Publicaciones por ZA",
         x = "Fecha",
         y = "Mediana de valor",
         color = "Zona y Ambientes") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica de la mediana de valor de las publicaciones por ZA
output$table_mediana_ZA <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_mediana_ZA,
                fecha %in% input$fecha_mediana_ZA,
                Ambientes %in% input$ambientes_mediana_ZA) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha, Zona_Agrupada, Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Ambientes, values_from = precio_pesos, values_fill = list(precio_pesos = NA)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))
  
if (nrow(datos_filtrados) == 0) {
        return(NULL)  # No mostrar nada si no hay datos
    }
  reactable(datos_filtrados)
})

# Gráfico del RIPTE
output$plot_RIPTE <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_RIPTE,
                fecha %in% input$fecha_RIPTE,
                Ambientes == "2 ambientes") %>%
    left_join(
      ripte, by = "fecha"
    ) %>%
    mutate (precio_pesos = as.numeric(gsub("\\.", "", precio_pesos)),
      ratio = precio_pesos / promedio) %>%
    group_by(fecha, Zona_Agrupada) %>% # Calcular la relación
    summarise(ratio = median(ratio, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = ratio, color = Zona_Agrupada, group = Zona_Agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = scales::percent(ratio, accuracy = 0.1)), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
    labs(title = "Incidencia del costo del alquiler en el salario (RIPTE)",
         x = "Mes",
         y = "Pesos/RIPTE") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
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

# Gráfico del SMVM
output$plot_SMVM <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter( Zona_Agrupada %in% input$zona_SMVM,
                fecha %in% input$fecha_SMVM,
                Ambientes == "2 ambientes") %>%
    left_join(
      smvm, by = "fecha"
    ) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos)),
            ratio_smvm = promedio / precio_pesos) %>%
    group_by(fecha, Zona_Agrupada) %>% # Calcular la relación
    summarise(ratio_smvm = median(ratio_smvm, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = ratio_smvm, color = Zona_Agrupada, group = Zona_Agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = scales::percent(ratio_smvm, accuracy = 0.1)), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) + 
    labs(title = "Incidencia del costo del alquiler en el salario (SMVM)",
         x = "Mes",
         y = "Pesos/SMVM") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
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

#--- Macrozonas de Desarrollo Prioritario ---#

# Cantidad de publicaciones de venta
output$plot_MDP_pub <- renderPlot({
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_pub, 
           fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Propiedad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = Propiedad, group = Propiedad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Cantidad de Publicaciones de Venta
output$table_MDP_pub <- renderReactable({
 datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter(Propiedad %in% input$propiedad_MDP_pub, 
    fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Propiedad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})


# Cantidad de publicaciones de departamentos en alquiler
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
    labs(title = "Cantidad de publicaciones",
         x = "Fecha",
         y = "Cantidad de Publicaciones") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
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

#Mediana de valor del m2 en venta
output$plot_MDP_mediana_m2 <- renderPlot({
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter( Propiedad %in% input$propiedad_MDP_mediana_m2,
            fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(Propiedad, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE),0), .groups = "drop")
#Grafico
  ggplot(datos_filtrados, aes(x = fecha, y = dolar_m2t, color = Propiedad, group = Propiedad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = scales::label_dollar(accuracy = 1)(dolar_m2t)), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
    labs(title = "Mediana de valor (US$)",
         x = "Fecha",
         y = "Mediana de valor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica de la mediana de valor del m2 en venta
output$table_MDP_mediana_m2 <- renderReactable({
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    rename(Propiedad = Tipo_propiedad) %>%
    filter( Propiedad %in% input$propiedad_MDP_mediana_m2,
            fecha %in% input$fecha_MDP_mediana_m2) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha, Propiedad) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE),0), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = dolar_m2t, values_fill = list(dolar_m2t = 0)) %>%
    mutate(across(where(is.numeric), ~ scales::label_dollar(accuracy = 1)(.)))
  

  reactable(datos_filtrados)
})


# Mediana de valor en departamentos de alquiler
output$plot_MDP_mediana <- renderPlot({
  datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha, Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    mutate(precio_pesos_label = scales::label_dollar()(precio_pesos))
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = precio_pesos, color = Ambientes, group = Ambientes)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = precio_pesos_label), vjust = -0.5, size = 4) +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(title = "Mediana de valor (en pesos ARS) de las publicaciones de departamentos en alquiler",
         x = "Fecha",
         y = "Mediana de valor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Mediana de valor de departamentos en alquiler
output$table_MDP_mediana <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    filter(Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    group_by(fecha,Ambientes) %>%
    filter(n() >= 20) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = precio_pesos, values_fill = list(precio_pesos = 0)) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), NA, scales::label_dollar()(.))))
    reactable(datos_filtrados)
})
