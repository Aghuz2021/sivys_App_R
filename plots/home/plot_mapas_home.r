
# Función para filtrar según tipo de propiedad y operación
# Cargar el script de gráficos
output$mapa <- renderLeaflet({
  # Crear el mapa base con las localidades
  leaflet() %>%
    addTiles() %>%  # Mapa base
    addPolygons(
      data = tres_de_febrero,
      color = "#0f6096",  # Color único para todos los bordes
      fillColor = "#3498db85",  # Color único para todos los rellenos
      weight = 1,  # Grosor de los bordes
      opacity = 1,  # Opacidad de los bordes
      fillOpacity = 0.7,  # Opacidad del relleno
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FF5733",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste("Barrio: ", nombre),  # Etiqueta con el nombre de la localidad
      layerId = ~nombre  # Identificador único para cada polígono (localidad)
    ) %>%
    addLegend(
      position = "topright",  # Ubicación de la leyenda (arriba a la derecha)
      colors = rep("#ffffff00", length(unique(tres_de_febrero$nombre))),  # Un color repetido para cada localidad
      labels = unique(tres_de_febrero$nombre),  # Nombres de todas las localidades
      title = "Localidades",  # Título de la leyenda
      opacity = 1  # Opacidad de la leyenda
    )
})
output$macrozonas <- renderLeaflet({
  # Filtrar los datos para mostrar solo la macrozona "Desarrollo Prioritario"
  macrozona_filtrada <- macrozonas %>%
    filter(macrozona == "Desarrollo Prioritario")

  # Crear el mapa base con las localidades filtradas
  leaflet() %>%
    addTiles() %>%  # Mapa base
    addPolygons(
      data = macrozona_filtrada,
      color = "#6e0372",  # Color único para todos los bordes
      fillColor = "#ec17f385",  # Color único para todos los rellenos
      weight = 1,  # Grosor de los bordes
      opacity = 1,  # Opacidad de los bordes
      fillOpacity = 0.7,  # Opacidad del relleno
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FF5733",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste(macroz_des)  # Etiqueta con la descripción de la macrozona
    ) %>%
    
    # Agregar el borde de Tres de Febrero
    addPolygons(
      data = tres_de_febrero,
      color = "#2E86C1",  # Color de los bordes de Tres de Febrero
      fillColor = NA,  # Sin relleno para Tres de Febrero (solo borde)
      weight = 2,  # Grosor de los bordes
      opacity = 1
    )%>%
    addLegend(
      position = "topright",  # Ubicación de la leyenda (arriba a la derecha)
      colors = "#6e0372",  # Color violeta para la leyenda
      labels = "Macrozonas de Desarrollo Prioritario",  # Etiqueta en la leyenda
      opacity = 1  # Opacidad de la leyenda
    )
})