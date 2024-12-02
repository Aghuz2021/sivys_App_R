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
             menuSubItem("", tabName = "alquiler_nuevo")
    ),
    menuItem("Macrozonas Desarrollo Prioritario", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "alquiler_Depto"),
             menuSubItem("", tabName = "alquiler_nuevo")
    )
  )
)