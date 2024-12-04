sidebarUI <- dashboardSidebar(
  width = "360px",
  sidebarMenu(
    menuItem("Home", 
              icon = icon("home"), 
              startExpanded = FALSE,
              tabName = "Home"

    ),
    menuItem("Venta de casas", icon = icon("home"), startExpanded = FALSE,
             menuSubItem("1.Cantidad de publicaciones", tabName = "venta_casa"),
             menuSubItem("2.Cantidad de m² publicados", tabName = "Cantidad_m²"),
             menuSubItem("3.Mediana de valor en usd por localidad.", tabName = "MEDIANA"),
             menuSubItem("4.Mediana de valor en usd por Zona agrupada", tabName = "mediana_za_casa"),
            menuSubItem("luu", tabName = "mediana_za_casa")
            
             

    ),
    menuItem("Venta de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "cantidad_public_depto"),
             menuSubItem("Cantidad de publicaciones de ambientes por ZA", tabName = "depto_nuevo"),
             menuSubItem("Cantidad de m²  publicados", tabName = "depto_nuevo"),
             menuSubItem("Mediana de valor usd por localidad", tabName = "depto_nuevo"),
             menuSubItem("Mediana de valor usd por ZA", tabName = "depto_nuevo"),
             menuSubItem("Mediana del valor (en US$) del m² por ZA", tabName = "depto_nuevo"),
             menuSubItem("(RIPTE) necesarios para comprar un depto de 100 m²", tabName = "depto_nuevo"),
             menuSubItem("(SMVM) necesarios para comprar un depto de 45 m²", tabName = "depto_nuevo")
    ),
    menuItem("Alquiler de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "alquiler_Depto"),
             menuSubItem("Cantidad de m²", tabName = "alquiler_m2"),
             menuSubItem("Cantidad de m² por ZA", tabName = "alquiler_m2_ZA"),
             menuSubItem("Mediana de valor de las publicaciones por ZA", tabName = "alquiler_mediana_ZA"),
             menuSubItem("(RIPTE) necesarios para comprar un depto de 45 m²", tabName = "alquiler_RIPTE"),
             menuSubItem("(SMVM) necesarios para comprar un depto de 45 m²", tabName = "alquiler_SMVM")
    ),
    menuItem("Macrozonas Desarrollo Prioritario", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "MDP_pub_venta"),
             menuSubItem("Cantidad de publicaciones en departamentos", tabName = "MDP_depto_alq"),
             menuSubItem("Mediana de valor (en US$) del m²", tabName = "MDP_mediana_m2"),
             menuSubItem("Mediana de valor (en pesos) de las publicaciones", tabName = "MDP_mediana")
    )
  )
)