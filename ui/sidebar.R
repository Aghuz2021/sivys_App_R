sidebarUI <- dashboardSidebar(
  width = "360px",
  sidebarMenu(
    menuItem("Home", 
              icon = icon("home"), 
              startExpanded = FALSE,
              tabName = "Home"

    ),
    menuItem("Venta de casas", icon = icon("home"), startExpanded = FALSE,
            menuSubItem("1. Cantidad de publicaciones", tabName = "venta_casa", icon = NULL),
            menuSubItem("2. Cantidad de m² publicados", tabName = "Cantidad_m²", icon = NULL), 
            menuSubItem("3. Mediana de valor en usd por localidad.", tabName = "MEDIANA", icon = NULL),
            menuSubItem("4. Mediana de valor en usd por Zona agrupada", tabName = "mediana_za_casa", icon = NULL),
            menuSubItem("(corregir) Mediana de valor en usd del m² por ZA.", tabName = "mediana_za_m2_casa", icon = NULL),
            menuSubItem("(corregir)Mediana de valor en usd del m² por localidad", tabName = "mediana_localidad_m2_casa", icon = NULL),
            menuSubItem("Ripte", tabName = "ripte_casa_venta", icon = NULL)
             
    ),
    menuItem("Venta de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "cantidad_public_depto"),
             menuSubItem("Cantidad de publicaciones de ambientes por ZA", tabName = "publicaciones_ambiente_za_depto"),
             menuSubItem("Cantidad de m²  publicados", tabName = "depto_m2_publicados"),
             menuSubItem("Mediana de valor usd por localidad", tabName = "depto_mediana_localidad"),
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