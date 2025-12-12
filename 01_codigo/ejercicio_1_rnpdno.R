# Ejercicio 1: Creación de base de datos
# Miguel Vázquez

#Setup ----
pacman::p_load(tidyverse, janitor, jsonlite, purrr, readr)

# Especificar locale---
Sys.setlocale("LC_ALL", "UTF-8")

# Deshabilitar notación científica---
options(scipen = 999)

# Carga path de archivos
edos <- list.files("02_input/salidas/", pattern = "\\.json$", full.names = TRUE)

# Función para seleccionar y leer las tablas en tibble por estado uniéndolas en una

leer_por_edad <- function(ruta, estado) {
  j <- fromJSON(ruta, flatten = TRUE)
    map_dfr(names(j$por_edad), function(sex_label) {
    edades_lista <- j$por_edad[[sex_label]]
    tibble(
      estado = estado,
      sexo   = sex_label,
      edad   = as.integer(names(edades_lista)),
      conteo = as.numeric(unlist(edades_lista))
    )
  })
}

# aplicar función a path con los 32 archivos
df_edades <- map_dfr(edos, function(ruta) {
  estado <- sub(".*/estado_(\\d{2})\\.json$", "\\1", ruta)
  leer_por_edad(ruta, estado)
})

# crear categorías de edad y reetiquetar estados

df_edades <- df_edades %>%
  filter(edad <= 113) %>% # la persona más longeva registrada en México tiene 114 años. más de eso es error.
  mutate(grupo_edad = case_when(edad >= 0  & edad <= 9   ~ "Infancias",
                                edad >= 10 & edad <= 19  ~ "Adolescentes",
                                edad >= 20 & edad <= 35  ~ "Jóvenes",
                                edad >= 36 & edad <= 59  ~ "Adultos",
                                edad >= 60 ~ "Adultos mayores",
                                TRUE ~ NA)) %>% 
  mutate(edo_nom = case_when(estado == "01" ~ "Aguascalientes",
                             estado == "02" ~ "Baja California",
                             estado == "03" ~ "Baja California Sur",
                             estado == "04" ~ "Campeche",
                             estado == "05" ~ "Coahuila",
                             estado == "06" ~ "Colima",
                             estado == "07" ~ "Chiapas",
                             estado == "08" ~ "Chihuahua",
                             estado == "09" ~ "Ciudad de México",
                             estado == "10" ~ "Durango",
                             estado == "11" ~ "Guanajuato",
                             estado == "12" ~ "Guerrero",
                             estado == "13" ~ "Hidalgo",
                             estado == "14" ~ "Jalisco",
                             estado == "15" ~ "México",
                             estado == "16" ~ "Michoacán",
                             estado == "17" ~ "Morelos",
                             estado == "18" ~ "Nayarit",
                             estado == "19" ~ "Nuevo León",
                             estado == "20" ~ "Oaxaca",
                             estado == "21" ~ "Puebla",
                             estado == "22" ~ "Querétaro",
                             estado == "23" ~ "Quintana Roo",
                             estado == "24" ~ "San Luis Potosí",
                             estado == "25" ~ "Sinaloa",
                             estado == "26" ~ "Sonora",
                             estado == "27" ~ "Tabasco",
                             estado == "28" ~ "Tamaulipas",
                             estado == "29" ~ "Tlaxcala",
                             estado == "30" ~ "Veracruz",
                             estado == "31" ~ "Yucatán",
                             estado == "32" ~ "Zacatecas",
                             TRUE ~ NA),
         sexo = case_when(sexo == "Mujeres" ~ "Femenino", # cambiar categorías
                          sexo == "Hombres" ~ "Masculino",
                       T ~ sexo)) %>% 
  select(edo_nom, sexo, grupo_edad, conteo)

# colapsar datos para que cada fila sea entidad/sexo/grupo_edad

df_final <- df_edades %>% 
  ungroup() %>% 
  group_by(edo_nom, sexo, grupo_edad) %>% 
  summarise(edo_nom = first(edo_nom), 
            sexo = first(sexo), 
            grupo_edad = first(grupo_edad), 
            conteo = sum(conteo))
  
  # Guardar datos en .csv
  write_csv(df_final, "03_output/rnpdno_2024.csv")

