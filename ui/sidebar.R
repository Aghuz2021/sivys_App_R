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
            menuSubItem("7. Ripte", tabName = "ripte_casa_venta", icon = NULL),
            menuSubItem("8. SMVM", tabName = "SMVM_casa_venta", icon = NULL)
             
    ),
    menuItem("Venta de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("Cantidad de publicaciones", tabName = "cantidad_public_depto"),
             menuSubItem("Cantidad de publicaciones de ambientes por ZA", tabName = "publicaciones_ambiente_za_depto"),
             menuSubItem("Cantidad de m²  publicados", tabName = "depto_m2_publicados"),
             menuSubItem("Mediana de valor usd por localidad", tabName = "depto_mediana_localidad"),
             menuSubItem("Mediana de valor usd por ZA", tabName = "depto_mediana_za"),
             menuSubItem("Mediana del valor (en US$) del m² por ZA", tabName = "depto_nuevo"),
             menuSubItem("(RIPTE) necesarios para comprar un depto de 100 m²", tabName = "depto_nuevo"),
             menuSubItem("(SMVM) necesarios para comprar un depto de 45 m²", tabName = "smvm_Depto_venta")
    ),
    menuItem("Alquiler de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("1. Cantidad de publicaciones", tabName = "cant_alquiler_Depto", icon  = NULL),
             menuSubItem("2. Cantidad de m²", tabName = "alquiler_m2", icon  = NULL),
             menuSubItem("3. Cantidad de m² por ZA", tabName = "alquiler_m2_ZA", icon  = NULL),
             menuSubItem("4. Mediana de valor de las publicaciones por ZA", tabName = "alquiler_mediana_ZA", icon  = NULL),
             menuSubItem("5. (RIPTE) necesarios para comprar un depto de 45 m²", tabName = "alquiler_RIPTE", icon  = NULL),
             menuSubItem("6. (SMVM) necesarios para comprar un depto de 45 m²", tabName = "alquiler_SMVM", icon  = NULL)
    ),
    menuItem("Macrozonas Desarrollo Prioritario", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("1. Cantidad de publicaciones", tabName = "MDP_pub_venta", icon  = NULL),
             menuSubItem("2. Cantidad de publicaciones en departamentos", tabName = "MDP_depto_alq", icon  = NULL),
             menuSubItem("3. Mediana de valor (en US$) del m²", tabName = "MDP_mediana_m2", icon  = NULL),
             menuSubItem("4. Mediana de valor (en pesos) de las publicaciones", tabName = "MDP_mediana", icon  = NULL)
    )
  )
)