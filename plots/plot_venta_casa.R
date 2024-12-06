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
    filter(nombre %in% input$localidad_depto, 
           fecha %in% input$fecha_depto) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Departamentos en Alquiler por Fecha y Localidad",
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
output$table_alq <- renderReactable({
 datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    filter(nombre %in% input$localidad_depto, 
    fecha %in% input$fecha_depto) %>%
    group_by(fecha, nombre) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)
    reactable(datos_filtrados)
})

#Gráfico de la cantidad de m2
output$plot_alq_m2 <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter( nombre %in% input$localidad_depto_m2, fecha %in% input$fecha_depto_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = nombre, group = nombre)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "Cantidad Total de m² Publicados de Departamentos en Alquiler por Fecha y Localidad",
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
        filter( nombre %in% input$localidad_depto_m2, fecha %in% input$fecha_depto_m2)%>%
    group_by(fecha, nombre) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})


#Grafico de la cantidad de m2 por Zona Agrupada
output$plot_alq_m2_ZA <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter( zona_agrupada %in% input$zona_depto_m2_ZA,
                fecha %in% input$fecha_depto_m2_ZA) %>%
    group_by(fecha, zona_agrupada) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = total_area, color = zona_agrupada, group = zona_agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = total_area), vjust = -0.5, size = 4) +
    labs(title = "Cantidad Total de m² de las Publicaciones de Departamentos en Alquiler por ZA",
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
        filter( zona_agrupada %in% input$zona_depto_m2_ZA, 
                fecha %in% input$fecha_depto_m2_ZA)%>%
    group_by(fecha, zona_agrupada) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_filtrados)
})


#Grafico de la mediana de valor de las publicaciones por ZA
output$plot_mediana_ZA <- renderPlot({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter( zona_agrupada %in% input$zona_mediana_ZA,
                fecha %in% input$fecha_mediana_ZA,
                Ambientes %in% input$ambientes_mediana_ZA) %>%
    mutate(precio_pesos = as.numeric(precio_pesos)) %>%
    group_by(fecha, zona_agrupada) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop")
  
  ggplot(datos_filtrados, aes(x = fecha, y = precio_pesos, color = zona_agrupada, group = zona_agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = precio_pesos), vjust = -0.5, size = 4) +
    labs(title = "Mediana de valor (en pesos ARS) de las Publicaciones de Departamentos en Alquiler Según Cantidad de Ambientes en ZA",
         x = "Fecha",
         y = "Mediana de valor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})

# Tabla dinámica de la cantidad de m2 por Zona Agrupada
output$table_mediana_ZA <- renderReactable({
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
        filter( zona_agrupada %in% input$zona_mediana_ZA,
                fecha %in% input$fecha_mediana_ZA,
                Ambientes %in% input$ambientes_mediana_ZA) %>%
    mutate(precio_pesos = as.numeric(precio_pesos)) %>%
    group_by(fecha, zona_agrupada) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = fecha, values_from = precio_pesos, values_fill = list(precio_pesos = 0))
  
  reactable(datos_filtrados)
})

#Gráfico de la Incidencia del Costo del Alquiler en el Salario RIPTE
output$plot_RIPTE <- renderPlot({
  # Filtrar y calcular la mediana del alquiler en pesos
  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    mutate(precio_pesos = as.numeric(precio_pesos)) %>%  # Convertir a numérico
    filter(!is.na(precio_pesos),
           Ambientes == "2 ambientes",
           fecha %in% input$fecha_RIPTE,
           zona_agrupada %in% input$zona_RIPTE) %>%
    group_by(fecha, zona_agrupada) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop")

print(datos_filtrados)

ripte <- ripte %>%
  rename(Fecha_publicacion = fecha)

  # Unir con los datos de RIPTE
  datos_con_ripte <- datos_filtrados %>%
    left_join(ripte, by = "Fecha_publicacion") %>%  # Unión por fecha
    mutate( RIPTE = as.numeric(RIPTE),  # Asegúrate de que RIPTE sea numérico
    precio_pesos_ripte = ifelse(is.na(RIPTE) | RIPTE == 0, NA, precio_pesos / RIPTE)  # Evitar divisiones por 0
  )    # Calcular la relación
  
  # Crear el gráfico
  ggplot(datos_con_ripte, aes(x = fecha_publicacion, y = precio_pesos_ripte, color = zona_agrupada, group = zona_agrupada)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_pesos_ripte, 2)), vjust = -0.5, size = 4) +
    labs(title = "Incidencia del costo del alquiler en el salario RIPTE",
         x = "Fecha",
         y = "Porcentaje") +
    theme_minimal()
})

#Tabla dinamica de la incidencia del costo RIPTE
output$table_RIPTE <- renderReactable({
  # Filtrar, calcular y unir con RIPTE
  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    mutate(precio_pesos = as.numeric(precio_pesos)) %>%  # Convertir a numérico
    filter(!is.na(precio_pesos),
           Ambientes == "2 ambientes",
           fecha %in% input$fecha_RIPTE,
           zona_agrupada %in% input$zona_RIPTE) %>%
    group_by(fecha, zona_agrupada) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), .groups = "drop") %>%
    left_join(ripte, by = "fecha") %>%  # Unión por fecha
    mutate(precio_pesos_ripte = precio_pesos / RIPTE) %>%  # Calcular la relación
    pivot_wider(names_from = fecha, values_from = precio_pesos_ripte, values_fill = 0)

  reactable(datos_filtrados)
})

#--- Macrozonas de Desarrollo Prioritario ---#

# Cantidad de publicaciones de venta
output$plot_MDP_pub <- renderPlot({
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
  # Filtrar datos según "Casa" y "Venta" y aplicar filtros adicionales
    filter(Tipo_propiedad %in% input$propiedad_MDP_pub, 
           fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Tipo_propiedad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = Tipo_propiedad, group = Tipo_propiedad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Viviendas en Venta Según Tipo de Propiedad",
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
    filter(Tipo_propiedad %in% input$propiedad_MDP_pub, 
    fecha %in% input$fecha_MDP_pub) %>%
    group_by(fecha, Tipo_propiedad) %>%
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
  ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = fecha, group = fecha)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
    labs(title = "Cantidad de Publicaciones de Departamentos en Alquiler",
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


# Mediana de valor del m2 en venta
output$plot_MDP_mediana_m2 <- renderPlot({
  datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    filter(Tipo_propiedad %in% input$propiedad_MDP_mediana_m2,
           fecha %in% input$fecha_MDP_mediana_m2) %>%
    group_by(fecha, Tipo_propiedad) %>%
    summarise(
      dolares_m2total = median(dolares_m2total, na.rm = TRUE), 
      .groups = "drop"
      ) %>%
    filter(!is.na(dolares_m2total))
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = dolares_m2total, color = Tipo_propiedad, group = Tipo_propiedad)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = round(dolares_m2total, 2)), vjust = -0.5, size = 4) +
    labs(title = "Mediana de valor (en US$) del m² de las publicaciones de viviendas en venta según tipo de propiedad",
         x = "Fecha",
         y = "Mediana de valor del m²") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
})


# Tabla dinámica para Mediana de valor del m2 en venta
output$table_MDP_mediana_m2 <- renderReactable({
 datos_filtrados <- filtro_operacion("DP", "Venta") %>%
    filter(Tipo_propiedad %in% input$propiedad_MDP_mediana_m2,
           fecha %in% input$fecha_MDP_mediana_m2) %>%
    group_by(fecha, Tipo_propiedad) %>%
    summarise(dolares_m2total = median(dolares_m2total, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
    filter(!is.na(dolares_m2total)) %>%
    pivot_wider(names_from = fecha, values_from = dolares_m2total, values_fill = 0)
    reactable(datos_filtrados)
})


# Mediana de valor en departamentos de alquiler
output$plot_MDP_mediana <- renderPlot({
  datos_filtrados <- filtro_propiedad_operacion_DP("Departamento", "Alquiler", "DP") %>%
    mutate(precio_pesos = gsub("[^0-9\\.]", "", precio_pesos),  # Limpieza de texto
           precio_pesos = as.numeric(precio_pesos)) %>%
    filter(!is.na(precio_pesos),
           Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    group_by(fecha, Ambientes) %>%
    summarise(
      precio_pesos = median(precio_pesos, na.rm = TRUE), 
      .groups = "drop"
      ) %>%
    filter(!is.na(precio_pesos))
  
  # Crear el gráfico
  ggplot(datos_filtrados, aes(x = fecha, y = precio_pesos, color = Ambientes, group = Ambientes)) +
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = round(precio_pesos, 3)), vjust = -0.5, size = 4) +
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
    mutate(precio_pesos = gsub("[^0-9\\.]", "", precio_pesos),  # Limpieza de texto
           precio_pesos = as.numeric(precio_pesos)) %>%
    filter(!is.na(precio_pesos),
           Ambientes %in% input$ambientes_MDP_mediana,
           fecha %in% input$fecha_MDP_mediana) %>%
    group_by(fecha,Ambientes) %>%
    summarise(precio_pesos = median(precio_pesos, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
    filter(!is.na(precio_pesos)) %>%
    pivot_wider(names_from = fecha, values_from = precio_pesos, values_fill = 0)
    reactable(datos_filtrados)
})