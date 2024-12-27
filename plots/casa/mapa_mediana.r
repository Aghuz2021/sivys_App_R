# Renderizar el mapa con Leaflet
output$mapa2 <- renderLeaflet({
  # Calcular la mediana para cada localidad
  medianas_por_localidad <- archivo %>%
    filter(Tipo_propiedad == "Casa", Tipo_operacion == "Venta") %>%
    group_by(nombre) %>%
    summarise(
      mediana_usd = if_else(n() > 20, median(precio_dolares, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    filter(!is.na(mediana_usd))
  
  # Unir las medianas al GeoJSON
  tres_de_febrero <- tres_de_febrero %>%
    left_join(medianas_por_localidad, by = "nombre")
  
  # Crear el mapa base con colores según la mediana
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = tres_de_febrero,
      color = "#0f6096",
      fillColor = ~colorNumeric("YlOrRd", mediana_usd)(mediana_usd),  # Escala de colores por mediana
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FF5733",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste(
        "Barrio: ", nombre,
        "<br>Mediana USD: ", round(mediana_usd, 2)
      ),  # Etiqueta con nombre y mediana
      layerId = ~nombre
    ) %>%
    addLegend(
      position = "topright",
      pal = colorNumeric("YlOrRd", tres_de_febrero$mediana_usd),
      values = tres_de_febrero$mediana_usd,
      title = "Mediana USD",
      opacity = 1
    )
})