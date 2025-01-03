sidebarUI <- dashboardSidebar(
  width = "360px",
  sidebarMenu(
    menuItem("Home", 
              icon = icon("home"), 
              startExpanded = FALSE,
              tabName = "Home"

    ),
    menuItem("Venta de casas", icon = icon("home"), startExpanded = FALSE,
             menuSubItem("1.Cantidad de publicaciones", tabName = "venta_casa", icon = NULL),
             menuSubItem("2.Cantidad de m² publicados", tabName = "Cantidad_m²", icon = NULL),
             menuSubItem("3.Mediana de valor en usd por localidad.", tabName = "MEDIANA", icon = NULL),
             menuSubItem("4.Mediana de valor en usd por Zona agrupada", tabName = "mediana_za_casa", icon  = NULL),
             menuSubItem("5.Mediana de valor por m² en usd por Zona Agrupada", tabName = "mediana_m2_za_casa", icon = NULL),
             menuSubItem("6.Mediana de valor por m² en usd por localidad", tabName = "mediana_m2_casa", icon = NULL),
             menuSubItem("7.(RIPTE) necesarios para comprar un casa de 100 m²", tabName = "casa_RIPTE", icon = NULL),
             menuSubItem("8.(SMVM) necesarios para comprar un casa de 45 m²", tabName = "casa_SMVM", icon = NULL)
    ),
    menuItem("Venta de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("1.Cantidad de publicaciones", tabName = "cantidad_public_depto", icon  = NULL),
             menuSubItem("2.Cantidad de publicaciones de ambientes por ZA", tabName = "cant_amb_za_depto", icon  = NULL),
             menuSubItem("3.Cantidad de m²  publicados", tabName = "depto_nuevo", icon  = NULL),
             menuSubItem("4.Mediana de valor usd por localidad", tabName = "depto_nuevo", icon  = NULL),
             menuSubItem("5.Mediana de valor usd por ZA", tabName = "depto_nuevo", icon  = NULL),
             menuSubItem("6.Mediana del valor (en US$) del m² por ZA", tabName = "depto_nuevo", icon  = NULL),
             menuSubItem("7.(RIPTE) necesarios para comprar un depto de 100 m²", tabName = "depto_nuevo", icon  = NULL),
             menuSubItem("8.(SMVM) necesarios para comprar un depto de 45 m²", tabName = "depto_nuevo", icon  = NULL)
    ),
    menuItem("Alquiler de departamento", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("1.Cantidad de publicaciones", tabName = "alquiler_Depto", icon  = NULL),
             menuSubItem("2.Cantidad de m²", tabName = "alquiler_m2", icon  = NULL),
             menuSubItem("3.Cantidad de m² por ZA", tabName = "alquiler_m2_ZA", icon  = NULL),
             menuSubItem("4.Mediana de valor de las publicaciones por ZA", tabName = "alquiler_mediana_ZA", icon  = NULL),
             menuSubItem("5.(RIPTE) necesarios para comprar un depto de 45 m²", tabName = "alquiler_RIPTE", icon  = NULL),
             menuSubItem("6.(SMVM) necesarios para comprar un depto de 45 m²", tabName = "alquiler_SMVM", icon  = NULL)
    ),
    menuItem("Macrozonas Desarrollo Prioritario", icon = icon("building"), startExpanded = FALSE,
             menuSubItem("1.Cantidad de publicaciones", tabName = "MDP_pub_venta", icon  = NULL),
             menuSubItem("2.Cantidad de publicaciones en departamentos", tabName = "MDP_depto_alq", icon  = NULL),
             menuSubItem("3.Mediana de valor (en US$) del m²", tabName = "MDP_mediana_m2", icon  = NULL),
             menuSubItem("4.Mediana de valor (en pesos) de las publicaciones", tabName = "MDP_mediana", icon  = NULL)
    )
  )
)
