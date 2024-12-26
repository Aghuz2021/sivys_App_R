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
   
      "
      ))
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
                selectInput("fecha_casa", "Selecciona uno o más meses:", 
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
                selectInput("fecha_m2", "Selecciona uno o más meses:", 
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
                selectInput("fecha_mediana_usd", "Selecciona uno o más meses:", 
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
                selectInput("fecha_zona_agrupada_Casa", "Selecciona uno o más meses:", 
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
                selectInput("fecha_depto", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_depto", height = "300px")
              )
            )
    ),
    tabItem(tabName = "alquiler_Depto",
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
                title = "Cantidad de publicaciones de departamentos en alquiler por localidad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_depto", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_depto", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_alq", height = "300px")
              ),
              box(
                title = "Tabla dinámica de publicaciones de departamentos en alquiler por localidad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_alq")
              )
            )
    ),
    tabItem(tabName = "alquiler_m2",
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
                title = "Cantidad de m² de las publicaciones de departamentos en alquiler por localidad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("localidad_depto_m2", "Selecciona una o más localidades:", 
                            choices = localidades, multiple = TRUE, selected = localidades[1]),
                selectInput("fecha_depto_m2", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_alq_m2", height = "300px")
              ),
              box(
                title = "Tabla dinámica de m² de las publicaciones de departamentos en alquiler por localidad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_alq_m2")
              )
            )
    ),
    tabItem(tabName = "alquiler_m2_ZA",
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
                title = "Cantidad de m² de las publicaciones de departamentos en alquiler por ZA",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("zona_depto_m2_ZA", "Selecciona una o más localidades:", 
                            choices = zonas_agrupadas, multiple = TRUE, selected = zonas_agrupadas[1]),
                selectInput("fecha_depto_m2_ZA", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_alq_m2_ZA", height = "300px")
              ),
              box(
                title = "Tabla dinámica de m² de las publicaciones de departamentos en alquiler por ZA",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_alq_m2_ZA")
              )
            )
    ),
    tabItem(tabName = "alquiler_mediana_ZA",
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
                title = "Mediana de valor (en pesos ARS) de las publicaciones de departamentos en alquiler según cantidad de ambientes por ZA",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("zona_mediana_ZA", "Selecciona una o más localidades:", 
                            choices = zonas_agrupadas, multiple = TRUE, selected = zonas_agrupadas[1]),
                selectInput("fecha_mediana_ZA", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                selectInput("ambientes_mediana_ZA", "Selecciona la cantidad de ambientes:", 
                            choices = ambientes, multiple = TRUE, selected = ambientes[1]),
                plotOutput("plot_mediana_ZA", height = "300px")
              ),
              box(
                title = "Tabla dinámica de la mediana de valor (en pesos ARS) de las publicaciones de departamentos en alquiler según cantidad de ambientes por ZA",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_mediana_ZA")
              )
            )
    ),
    tabItem(tabName = "alquiler_RIPTE",
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
                title = "Incidecia del costo del alquiler en el salario (RIPTE)",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("zona_RIPTE", "Selecciona una o más zonas:", 
                            choices = zonas_agrupadas, multiple = TRUE, selected = zonas_agrupadas[1]),
                selectInput("fecha_RIPTE", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_RIPTE", height = "300px")
              ),
              box(
                title = "Tabla dinámica de RIPTE",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_RIPTE")
              )
            )
    ),
    tabItem(tabName = "alquiler_SMVM",
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
                title = "Incidencia del costo del alquiler en el salario (SMVM)",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("zona_SMVM", "Selecciona una o más zonas:", 
                            choices = zonas_agrupadas, multiple = TRUE, selected = zonas_agrupadas[1]),
                selectInput("fecha_SMVM", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_SMVM", height = "300px")
              ),
              box(
                title = "Tabla dinámica de SMVM",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_SMVM")
              )
            )
    ),
    tabItem(tabName = "MDP_pub_venta",
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
                title = "Cantidad de publicaciones de viviendas en venta según tipo de propiedad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("propiedad_MDP_pub", "Selecciona casa o departamento:", 
                            choices = tipo_de_propiedad, multiple = TRUE, selected = tipo_de_propiedad[1]),
                selectInput("fecha_MDP_pub", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_MDP_pub", height = "300px")
              ),
              box(
                title = "Tabla dinámica de cantidad de publicaciones de viviendas en venta según tipo de propiedad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_MDP_pub")
              )
            )
    ),
    tabItem(tabName = "MDP_depto_alq",
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
                title = "Cantidad de publicaciones de departamentos en alquiler",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("fecha_MDP_depto_alq", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_MDP_depto_alq", height = "300px")
              ),
              box(
                title = "Tabla dinámica de cantidad de publicaciones de departamentos en alquiler",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_MDP_depto_alq")
              )
            )
    ),
    tabItem(tabName = "MDP_mediana_m2",
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
                title = "Mediana de valor (en US$) del m² de las publicaciones de viviendas en venta según tipo de propiedad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("propiedad_MDP_mediana_m2", "Selecciona casa o departamento:", 
                            choices = tipo_de_propiedad, multiple = TRUE, selected = tipo_de_propiedad[1]),
                selectInput("fecha_MDP_mediana_m2", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                plotOutput("plot_MDP_mediana_m2", height = "300px")
              ),
              box(
                title = "Tabla dinámica de la mediana de valor (en US$) del m² de las publicaciones de viviendas en venta según tipo de propiedad",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_MDP_mediana_m2")
              )
            )
    ),
    tabItem(tabName = "MDP_mediana",
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
                title = "Mediana de valor (en pesos) de departamentos en alquiler",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput("fecha_MDP_mediana", "Selecciona uno o más meses:", 
                            choices = fechas_formateadas, multiple = TRUE, selected = fechas_formateadas[1]),
                selectInput("ambientes_MDP_mediana", "Selecciona la cantidad de ambientes:", 
                            choices = ambientes, multiple = TRUE, selected = ambientes[1]),
                plotOutput("plot_MDP_mediana", height = "300px")
              ),
              box(
                title = "Tabla dinámica de la mediana de valor (en pesos) de departamentos en alquiler",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("table_MDP_mediana")
              )
            )
    ),
    tabItem(
  tabName = "Home",
  fluidPage(
    # Estilos para mejorar la apariencia
    tags$style(HTML("
    .info-box-content  { 
        padding: 0px 26px;
        margin-left: 0px;
      }
    .info-box-content h3 {
    font-size: 24px;
    background: #4d6d7cf5;
    color: white;
    padding: 7px;
    }
      .info-box-content p { 
        font-size: 14px; 
        line-height: 1.5; 
      }
      .zones-container {
        display: flex; 
        flex-wrap: wrap; 
        gap: 20px; 
        justify-content: space-around;
      }
      .zone-column {
        flex: 1 1 calc(20% - 20px);
        min-width: 150px;
        border: 1px solid #3748504d;
        background: white;
        margin-top: 1 px solid;
        padding: 8px;
        border-radius: 10px;
      }
      .zone-column ul {
        padding-left: 20px; 
      }
      @media (max-width: 768px) {
        .zone-column {
          flex: 1 1 100%;
        }
      }
    ")),

      div(
        class = "info-box-content",
        h3("Cantidad de publicaciones según tipo de operación para el periodo noviembre 2023 a octubre 2024"),
        tableOutput("resumen_datos")
      ),

      div(
        class = "info-box-content",
        h3("Medianas de valor de venta (en US$) según tipo de propiedad durante el último mes"),
        tableOutput("tabla_mediana")
      ),

      div(
        class = "info-box-content",
        h3("Medianas de valor de alquileres (en ARS) de departamentos según cantidad de ambientes en Tres de Febrero durante el último mes"),
        tableOutput("tabla_alquiler")
    ),
    # Información descriptiva
    
      div(
        class = "info-box-content",
        HTML("
          <h3>Objetivos</h3>
          <p>
            El Sistema de Información de Vivienda y Suelo de Tres de Febrero (SIVyS) permite reunir, producir, analizar y difundir datos relacionados con la situación de la vivienda y el suelo del Municipio, 
            así como conocer la situación habitacional de las familias y cuantificar la valorización del suelo producto de la intervención pública municipal.
          </p>
          <p>
            Este Monitor Periódico de Indicadores tiene como objetivo relevar las dinámicas del mercado inmobiliario dentro del Partido de Tres de Febrero, 
            mediante la revisión de los datos recolectados en publicaciones (de operaciones de alquiler y compra-venta de viviendas) realizadas en Mercado Libre, 
            procesados por la Fundación Universidad de San Andrés y la Municipalidad para su mejor manipulación.
          </p>
          <h3>Fuente de información y tratamiento de la base de datos</h3>
          <p>
            Mensualmente, Mercado Libre envía a la Municipalidad los siguientes datos:
          </p>
          <ul>
            <li>Fecha de recolección de la publicación</li>
            <li>Carácter de la publicación (dueño directo o inmobiliaria)</li>
            <li>Tipo de operación (alquiler o venta)</li>
            <li>m² totales y m² cubiertos</li>
            <li>Cantidad de habitaciones y ambientes</li>
            <li>Tipo de propiedad (departamento o casa)</li>
            <li>Moneda de la publicación</li>
            <li>Precio en dólares y moneda local</li>
            <li>ID de la publicación</li>
            <li>Coordenadas del inmueble</li>
          </ul>
          <p>
           Recibido los datos, la Municipalidad realiza el siguiente procedimiento sobre los mismos:
          </p>
          <ul>
            <li>Asignación de localidades predefinidas</li>
            <li>Conversión de valores a dólar MEP</li>
            <li>Limpieza de datos poco fiables y eliminación de outliers</li>
            <li>Calculo de valores de m²</li>
            <li>Agrupación de localidades en Zonas Agrupadas</li>
          </ul>
          <h3>Zonas Agrupadas</h3>
          <p>
            Dado que algunas localidades tienen pocas publicaciones, se generaron agrupaciones en zonas para establecer muestras más representativas.
          </p>
          <div class='zones-container'>
            <div class='zone-column'>
              <h4>Norte</h4>
              <ul>
                <li>11 de Septiembre</li>
                <li>Churruca</li>
                <li>El Libertador</li>
                <li>Loma Hermosa</li>
                <li>Pablo Podestá</li>
                <li>Remedios de Escalada de San Martín</li>
              </ul>
            </div>
            <div class='zone-column'>
              <h4>Centro</h4>
              <ul>
                <li>Villa Bosch</li>
                <li>Martín Coronado</li>
                <li>Ciudad Jardín</li>
                <li>Barrio Altos de Podestá</li>
              </ul>
            </div>
            <div class='zone-column'>
              <h4>Sur</h4>
              <ul>
                <li>Ciudadela</li>
                <li>José Ingenieros</li>
                <li>Villa Raffo</li>
              </ul>
            </div>
            <div class='zone-column'>
              <h4>Este</h4>
              <ul>
                <li>Sáenz Peña</li>
                <li>Santos Lugares</li>
              </ul>
            </div>
            <div class='zone-column'>
              <h4>Caseros</h4>
              <ul>
                <li>Caseros</li>
              </ul>
            </div>
          </div>
          <h3>Macrozonas de Desarrollo Prioritario </h3>
          <p>Las Macrozonas de Desarrollo Prioritario (MDP) constituyen sectores o inmuebles, que estando desocupados o subutilizados, son objeto de políticas que buscan cohibir el uso especulativo y fomentar el desarrollo o construcción, de modo de aprovechar la existencia de infraestructura y expandir la oferta inmobiliaria para usos residenciales o comerciales. El objetivo de las MDP es concentrar el crecimiento urbano en densidades adecuadas y priorizando el completamiento del tejido urbano.</p>
        ")
      )
    )
  )
)


  )

    
  

