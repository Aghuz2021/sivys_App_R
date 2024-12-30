observeEvent(input$limpiar_casa, {
  updateSelectInput(session, "localidad_casa", selected = NULL)
  updateSelectInput(session, "fecha_casa", selected = NULL)
})