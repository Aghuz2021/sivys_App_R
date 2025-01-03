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

output$resumen_datos <- renderUI({
  # Calcular los datos de resumen
  total_datos <- nrow(archivo)  # Cambia 'tu_dataframe' por el nombre real de tu dataset
  total_alquiler <- sum(archivo$Tipo_operacion == "Alquiler", na.rm = TRUE)
  total_venta <- sum(archivo$Tipo_operacion == "Venta", na.rm = TRUE)
  
  # Crear el dataframe para mostrar en la tabla
  resumen_df <- data.frame(
    Indicador = c("Total de Publicaciones", "Cantidad de Publicaciones de Alquiler", "Cantidad de Publicaciones de Venta"),
    Descripción = c(total_datos, total_alquiler, total_venta)
  )

tabla_html <- resumen_df %>%
    knitr::kable("html", col.names = c("Indicador", "Descripción"), align = "c") %>%
    kableExtra::kable_styling("striped", full_width = FALSE, position = "center")
  
  # Renderizar como HTML
  HTML(tabla_html)
})

#Calcula la mediana por m2 y por inmueble
output$tabla_mediana <- renderUI({
  library(kableExtra)
  library(dplyr)
 
  # Filtrar los datos para "Venta"
  datos_filtrados <- archivo %>%
    filter(Tipo_operacion == "Venta") %>%
    mutate(precio_por_m2 = precio_dolares / Total_area) %>% 
    filter(!is.na(precio_por_m2), !is.na(precio_dolares), Total_area > 0) %>%
    mutate(Mes = substr(fecha, 1, 7))  # Extraer año-mes (yyyy-mm)
 
  # Filtrar para el mes más reciente
  mes_reciente <- max(datos_filtrados$Mes)  # Obtener el mes más reciente
  datos_filtrados_mes_reciente <- datos_filtrados %>%
    filter(Mes == mes_reciente)  # Filtrar los datos para el mes más reciente
 
 #Calculo las medianas y las localidades
  resumen_mediana <- datos_filtrados_mes_reciente %>%
  rename(Propiedad = Tipo_propiedad) %>%
  group_by(Propiedad) %>%
  summarise(
    Mediana_Precio_m2 = round(median(precio_por_m2, na.rm = TRUE), 0),
    # Localidad con la mediana más alta para el precio por m² por tipo de propiedad
    Localidad_Max_Precio_m2 = datos_filtrados_mes_reciente %>%
      filter(Tipo_propiedad == first(Propiedad)) %>%
      group_by(nombre) %>%
      summarise(Mediana_Precio_m2 = median(precio_por_m2, na.rm = TRUE)) %>%
      filter(Mediana_Precio_m2 == max(Mediana_Precio_m2, na.rm = TRUE)) %>%
      pull(nombre),
    Mediana_Precio_Inmueble = round(median(precio_dolares, na.rm = TRUE), 0),
    # Localidad con la mediana más alta para el precio de inmueble por tipo de propiedad
    Localidad_Max_Precio_Inmueble = datos_filtrados_mes_reciente %>%
      filter(Tipo_propiedad == first(Propiedad)) %>%
      group_by(nombre) %>%
      summarise(Mediana_Precio_Inmueble = median(precio_dolares, na.rm = TRUE)) %>%
      filter(Mediana_Precio_Inmueble == max(Mediana_Precio_Inmueble, na.rm = TRUE)) %>%
      pull(nombre),
    .groups = "drop"
  )
 
  # Crear la tabla HTML
  tabla_html <- resumen_mediana %>%
    kbl(col.names = c(
      "Tipo de inmueble",
      "Mediana de Valor en Tres de Febrero", "Localidad con Valor más Alto Registrado",
      "Mediana de Valor en Tres de Febrero", "Localidad con Valor más Alto Registrado"
    )) %>%
    add_header_above(c(
      " " = 1,
      "Precio de Venta por m²" = 2,
      "Precio de Venta de Inmuebles" = 2
    )) %>%
    kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover")) %>%
    as.character()
 
  HTML(tabla_html)
})


#Calcular la mediana de alquiler según cantidad de ambientes
output$tabla_alquiler <- renderUI({
  library(kableExtra)
  library(dplyr)

  # Paso 1: Verifica las columnas y datos
  req(archivo)  # Asegúrate de que el dataframe exista
  req(all(c("fecha", "precio_pesos", "Ambientes") %in% colnames(archivo)))  # Verifica columnas necesarias

  # Filtrar los datos para "Alquiler" y departamentos
  datos_filtrados <- archivo %>%
    filter(Tipo_operacion == "Alquiler", Tipo_propiedad == "Departamento") %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    filter(!is.na(precio_pesos)) %>%  # Excluir registros sin precio
    mutate(Mes = substr(fecha, 1, 7))  # Extraer año-mes (yyyy-mm)

  # Paso 2: Manejo de datos vacíos
  validate(
    need(nrow(datos_filtrados) > 0, "No hay datos para mostrar.")
  )

  # Filtrar para el mes más reciente
  mes_reciente <- max(datos_filtrados$Mes, na.rm = TRUE)  # Obtener el mes más reciente
  datos_filtrados_mes_reciente <- datos_filtrados %>%
    filter(Mes == mes_reciente)  # Filtrar los datos para el mes más reciente

  # Calcular la mediana de precio de alquiler por cantidad de ambientes
  resumen_mediana <- datos_filtrados_mes_reciente %>%
    group_by(Ambientes) %>%
    mutate(precio_pesos = as.numeric(gsub("\\.", "", precio_pesos))) %>%
    summarise(
      Mediana_Alquiler = median(precio_pesos, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Ambientes)  # Ordenar por cantidad de ambientes

  # Crear la tabla en formato HTML
  tabla_html <- resumen_mediana %>%
    kbl(col.names = c(
      "Cantidad de Ambientes",
      "Mediana de Valor de Alquiler"
    )) %>%
    kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover")) %>%
    as.character()

  # Paso 3: Renderizar el HTML
  div(HTML(tabla_html))
})

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


#Mediana de valor del m2 en venta por ZA
output$plot_mediana_m2_za_casa <- renderPlot({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(zona_agrupada = "Tres de Febrero")  # Asignar etiqueta de grupo
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(zona_agrupada %in% input$zona_mediana_m2_usd,
           fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(zona_agrupada, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)
  
  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = dolar_m2t, color = zona_agrupada, group = zona_agrupada)) +
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
output$table_mediana_m2_za_casa <- renderReactable({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(zona_agrupada = "Tres de Febrero")
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(zona_agrupada %in% input$zona_mediana_m2_usd,
           fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(zona_agrupada, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = dolar_m2t, values_fill = list(dolar_m2t = 0)) %>%
    mutate(across(where(is.numeric), ~ scales::label_dollar(accuracy = 1)(.)))
  
  # Crear la tabla reactable
  reactable(datos_combinados)
})


#Mediana de valor del m2 en venta por localidad
output$plot_mediana_m2_casa <- renderPlot({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(nombre = "Tres de Febrero")  # Asignar etiqueta de grupo
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_mediana_m2_usd,
           fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(nombre, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)
  
  # Crear el gráfico
  ggplot(datos_combinados, aes(x = fecha, y = dolar_m2t, color = nombre, group = nombre)) +
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
output$table_mediana_m2_casa <- renderReactable({
  # Calcular la mediana de valor del m² para "Tres de Febrero" (sin distinción por propiedad)
  tres_de_febrero <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop") %>%
    mutate(nombre = "Tres de Febrero")
  
  # Calcular la mediana de valor del m² para los datos filtrados
  datos_filtrados <- filtro_propiedad_operacion("Casa", "Venta") %>%
    filter(nombre %in% input$localidad_mediana_m2_usd,
           fecha %in% input$fecha_mediana_m2_usd) %>%
    mutate(dolar_m2t = precio_dolares / Total_area) %>%
    group_by(nombre, fecha) %>%
    filter(n() >= 20) %>%
    summarise(dolar_m2t = round(median(dolar_m2t, na.rm = TRUE), 0), .groups = "drop")
  
  # Combinar los datos
  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = dolar_m2t, values_fill = list(dolar_m2t = 0)) %>%
    mutate(across(where(is.numeric), ~ scales::label_dollar(accuracy = 1)(.)))
  
  # Crear la tabla reactable
  reactable(datos_combinados)
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


#Cantidad de publicaciones según cantidad de ambientes por ZA
output$plot_cant_amb_depto <- renderPlot({
    # Calcular la mediana de valor por Zona Agrupada y Ambientes
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Venta") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_depto,
               fecha %in% input$fecha_depto,
               Ambientes %in% input$ambientes_depto) %>%
        group_by(fecha, Zona_Agrupada, Ambientes) %>%
        summarise(conteo_publicaciones = n(), .groups = "drop")

    # Crear el gráfico
    ggplot(datos_filtrados, aes(x = fecha, y = conteo_publicaciones, color = interaction(Zona_Agrupada, Ambientes), group = interaction(Zona_Agrupada, Ambientes))) +
        geom_line() +
        geom_point(size = 2) +
        geom_text(aes(label = conteo_publicaciones), vjust = -0.5, size = 4) +
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
output$table_cant_amb_depto <- renderReactable({
    # Calcular la mediana de valor por Zona Agrupada y Ambientes
    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Venta") %>%
        rename(Zona_Agrupada = zona_agrupada) %>%
        filter(Zona_Agrupada %in% input$zona_depto,
               fecha %in% input$fecha_depto,
               Ambientes %in% input$ambientes_depto) %>%
        group_by(fecha, Zona_Agrupada, Ambientes) %>%
        summarise(conteo_publicaciones = n(), .groups = "drop") %>%
        pivot_wider(names_from = Ambientes, values_from = conteo_publicaciones, values_fill = NA)

    # Crear la tabla reactable
    reactable(datos_filtrados)
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
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto) %>%
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la localidad "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")

    datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto, 
           fecha %in% input$fecha_depto) %>%
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
    filter(fecha %in% input$fecha_depto) %>%
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la fila "Tres de Febrero"
              conteo_publicaciones = n(), .groups = "drop")

  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto, 
           fecha %in% input$fecha_depto) %>%
    group_by(fecha, Localidad) %>%
    summarise(conteo_publicaciones = n(), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = conteo_publicaciones, values_fill = 0)

    reactable(datos_combinados)
})

#Gráfico de la cantidad de m2
output$plot_alq_m2 <- renderPlot({
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_m2) %>%  # Filtrar según las fechas seleccionadas
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la línea "Tres de Febrero"
              total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")
            
  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_m2, 
           fecha %in% input$fecha_depto_m2) %>%
    group_by(fecha, Localidad) %>%
    summarise(total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero)

  datos_combinados <- datos_combinados %>%
    mutate(Localidad = factor(Localidad, levels = c("Tres de Febrero", unique(datos_filtrados$Localidad))))
  
  ggplot(datos_combinados, aes(x = fecha, y = total_area, color = Localidad, group = Localidad)) +
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

# Tabla dinámica para m² Publicados
output$table_alq_m2 <- renderReactable({
  tres_de_febrero <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(fecha %in% input$fecha_depto_m2) %>%  # Filtrar según las fechas seleccionadas
    group_by(fecha) %>%
    summarise(Localidad = "Tres de Febrero",  # Crear la fila "Tres de Febrero"
              Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_filtrados <- filtro_propiedad_operacion("Departamento", "Alquiler") %>%
    rename(Localidad = nombre) %>%
    filter(Localidad %in% input$localidad_depto_m2, 
           fecha %in% input$fecha_depto_m2) %>%
    group_by(fecha, Localidad) %>%
    summarise(Total_area = sum(Total_area, na.rm = TRUE), .groups = "drop")

  datos_combinados <- bind_rows(datos_filtrados, tres_de_febrero) %>%
    pivot_wider(names_from = fecha, values_from = Total_area, values_fill = 0)
  
  reactable(datos_combinados)
})


#Grafico de la cantidad de m2 por Zona Agrupada
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


#Grafico de la mediana de valor de las publicaciones por ZA
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

#--- Macrozonas de Desarrollo Prioritario ---#

# Cantidad de publicaciones de venta
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


# Mediana de valor en departamentos de alquiler
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




