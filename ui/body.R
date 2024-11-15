bodyUI <- dashboardBody(

   tags$head(
      tags
     
$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #374850; 
          color: #fff;
          border-bottom: 0 solid transparent; 
        }
        /* Estilos para el sidebar */
        .skin-blue .main-sidebar {
          background-color: #ffffff; /* Cambia este color según tus necesidades */
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a {
          color: ##000000; /* Cambia el color del texto de los elementos del sidebar */
        }
        .skin-blue .main-header .navbar{
          background-color: #374850; /* Cambia este color según tus necesidades */
        }
        .box.box-solid.box-primary>.box-header{
         background-color: #374850;
        }
        .skin-blue .sidebar a {
    color: #000000;
}
.modal-body{
font-size:15px
}
sidebar-menu{
background-color: #fffff;
}
.skin-blue .sidebar-menu>li>.treeview-menu {
    margin: 0 1px;
    background: #b1b1b11c;
    color: #000000
}
.skin-blue .treeview-menu>li.active>a, .skin-blue .treeview-menu>li>a {
    color: #000000;
}
.skin-blue .treeview-menu>li.active>a, .skin-blue .treeview-menu>li>a:hover {
    color: #3c8dbc;
}
.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    color: #fff;
    background: #486572;
    border-left-color: #ffa009;

}.skin-blue .sidebar-menu>li>a {
    border-left: 12px solid transparent;
}
   
      "))
    ),
  tabItems(
    # Tab para "Venta de casas"
    tabItem(tabName = "venta_casa",
            fluidRow(
              tags$style(HTML("
                .selectize-input { color: #007bff; background-color: #3c8dbc21; }
                .selectize-dropdown .option:hover { background-color: #3c8dbc21; }
                .selectize-control.multi .selectize-input>div {
                  cursor: pointer; margin: 0 3px 3px 0; padding: 1px 5px; 
                  background: #f9ffa5; color: #333; border: 0 solid rgba(0, 0, 0, 0);
                }
              ")),
              box(
                title = "Cantidad de publicaciones de ventas de casas",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_casa", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_casa", "Selecciona una o más fechas:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_casa", height = "300px")
              ),
              box(
                title = "Tabla dinámica de publicaciones de ventas de casas",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_casa") 
              )
            )
    ),
    
    # Tab para "Cantidad m²"
    tabItem(tabName = "Cantidad_m²",
            fluidRow(
              tags$style(HTML("
                .selectize-input { color: #007bff; background-color: #3c8dbc21; }
                .selectize-dropdown .option:hover { background-color: #3c8dbc21; }
                .selectize-control.multi .selectize-input>div {
                  cursor: pointer; margin: 0 3px 3px 0; padding: 1px 5px; 
                  background: #f9ffa5; color: #333; border: 0 solid rgba(0, 0, 0, 0);
                }
              ")),
              box(
                title = "Cantidad de publicaciones de ventas por metro cuadrado",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_m2", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_m2", "Selecciona una o más fechas:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_m2", height = "300px")
              ),
              box(
                title = "Tabla dinámica de publicaciones de ventas por metro cuadrado",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_m2")
              )
            )
    ),
    
    # Tab para "Mediana de precio en USD"
    tabItem(tabName = "MEDIANA",
            fluidRow(
              tags$style(HTML("
                .selectize-input { color: #007bff; background-color: #3c8dbc21; }
                .selectize-dropdown .option:hover { background-color: #3c8dbc21; }
                .selectize-control.multi .selectize-input>div {
                  cursor: pointer; margin: 0 3px 3px 0; padding: 1px 5px; 
                  background: #f9ffa5; color: #333; border: 0 solid rgba(0, 0, 0, 0);
                }
              ")),
              box(
                title = "Mediana de Precio en USD de Casas en Venta",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_mediana_usd", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_mediana_usd", "Selecciona una o más fechas:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_mediana_usd_por_localidad", height = "300px")
              ),
                box(
                title = "Tabla dinámica de publicaciones de ventas por metro cuadrado",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_usd_por_localidad")
              )
            )
    ),
    tabItem(tabName = "mediana_za_casa",
            fluidRow(
              tags$style(HTML("
                .selectize-input { color: #007bff; background-color: #3c8dbc21; }
                .selectize-dropdown .option:hover { background-color: #3c8dbc21; }
                .selectize-control.multi .selectize-input>div {
                  cursor: pointer; margin: 0 3px 3px 0; padding: 1px 5px; 
                  background: #f9ffa5; color: #333; border: 0 solid rgba(0, 0, 0, 0);
                }
              ")),
              box(
                title = " ",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("zona_agrupada_casa", "Seleccionar zonas agrupadas:", 
                            choices = zonas_agrupadas, multiple = TRUE, selected = zonas_agrupadas[1]),
                selectInput("fecha_zona_agrupada_Casa", "Selecciona una o más fechas:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_mediana_usd_por_za", height = "300px")
              )
            )
    ),
    tabItem(tabName = "cantidad_public_depto",
            fluidRow(
              tags$style(HTML("
                .selectize-input { color: #007bff; background-color: #3c8dbc21; }
                .selectize-dropdown .option:hover { background-color: #3c8dbc21; }
                .selectize-control.multi .selectize-input>div {
                  cursor: pointer; margin: 0 3px 3px 0; padding: 1px 5px; 
                  background: #f9ffa5; color: #333; border: 0 solid rgba(0, 0, 0, 0);
                }
              ")),
              box(
                title = "cantidad de publicciones depto",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_depto", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_depto", "Selecciona una o más fechas:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_depto", height = "300px")
              )
            )
    )
  )
)
