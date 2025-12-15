# Ejercicio 2: Análisis homicidios
# Miguel Vázquez

#Setup ----
pacman::p_load(tidyverse, purrr, readr, janitor, foreign, sf, showtext,
               sysfonts)

# Especificar locale---
Sys.setlocale("LC_ALL", "UTF-8")

# Deshabilitar notación científica---
options(scipen = 999)

# Carga path de archivo

hom_24 <- read.dbf("02_input/def_inegi_2024/DEFUN24.dbf") %>% 
  clean_names() %>% 
  select(ent_ocurr, tipo_defun, sexo, afromex, lengua, edad, escolarida, edo_civil) %>% 
  mutate(edad = case_when(edad >= 4001 & edad <= 4120 ~ as.numeric(edad - 4000),
                         edad >= 2001 & edad <= 2029 ~ (edad - 2000) / 365,
                         edad >= 3001 & edad <= 3011 ~ (edad - 3000) / 12,
                         edad >= 1001 & edad <= 1023 ~ 0,
                         edad == 1097 ~ 0,
                         edad %in% c(1098, 2098, 3098, 4998) ~ NA,
                         T ~ NA),
         edad = ifelse(edad <= 1, 0, edad),
         escolaridad_cat = case_when(
           escolarida == 1  ~ "Sin escolaridad",
           escolarida == 2  ~ "Preescolar",
           escolarida == 3  ~ "Primaria inc.",
           escolarida == 4  ~ "Primaria",
           escolarida == 5  ~ "Secundaria inc.",
           escolarida == 6  ~ "Secundaria",
           escolarida == 7  ~ "Prepa inc.",
           escolarida == 8  ~ "Prepa",
           escolarida == 9  ~ "Profesional",
           escolarida == 10 ~ "Posgrado",
           escolarida == 88 ~ "No aplica",
           escolarida == 99 ~ "No esp.",
           T ~ NA),
         edo_nom = case_when(
           ent_ocurr == "01" ~ "Aguascalientes",
           ent_ocurr == "02" ~ "Baja California",
           ent_ocurr == "03" ~ "Baja California Sur",
           ent_ocurr == "04" ~ "Campeche",
           ent_ocurr == "05" ~ "Coahuila",
           ent_ocurr == "06" ~ "Colima",
           ent_ocurr == "07" ~ "Chiapas",
           ent_ocurr == "08" ~ "Chihuahua",
           ent_ocurr == "09" ~ "Ciudad de México",
           ent_ocurr == "10" ~ "Durango",
           ent_ocurr == "11" ~ "Guanajuato",
           ent_ocurr == "12" ~ "Guerrero",
           ent_ocurr == "13" ~ "Hidalgo",
           ent_ocurr == "14" ~ "Jalisco",
           ent_ocurr == "15" ~ "México",
           ent_ocurr == "16" ~ "Michoacán",
           ent_ocurr == "17" ~ "Morelos",
           ent_ocurr == "18" ~ "Nayarit",
           ent_ocurr == "19" ~ "Nuevo León",
           ent_ocurr == "20" ~ "Oaxaca",
           ent_ocurr == "21" ~ "Puebla",
           ent_ocurr == "22" ~ "Querétaro",
           ent_ocurr == "23" ~ "Quintana Roo",
           ent_ocurr == "24" ~ "San Luis Potosí",
           ent_ocurr == "25" ~ "Sinaloa",
           ent_ocurr == "26" ~ "Sonora",
           ent_ocurr == "27" ~ "Tabasco",
           ent_ocurr == "28" ~ "Tamaulipas",
           ent_ocurr == "29" ~ "Tlaxcala",
           ent_ocurr == "30" ~ "Veracruz",
           ent_ocurr == "31" ~ "Yucatán",
           ent_ocurr == "32" ~ "Zacatecas",
           T ~ NA),
         afromex = case_when(afromex == "8" ~ NA, # No especificados o no aplica
                             afromex == "9" ~ NA,
                             afromex == "1" ~ "Afromexicanx",
                             afromex == "2" ~ "No afromexicanx",
                             T ~ NA),
         lengua = case_when(lengua == "8" ~ NA,
                            lengua == "9" ~ NA,
                            lengua == "1" ~ "Indigena",
                            lengua == "2" ~ "No indigena",
                            T ~ NA),
         sexo = case_when(sexo == 1 ~ "Masculino",
                         sexo == 2 ~ "Femenino",
                         T ~ "Otro"),
         pareja = case_when(
           edo_civil %in% c(4, 5) ~ "Con pareja",
           edo_civil %in% c(1, 2, 3, 6) ~ "Sin pareja",
           edo_civil %in% c(8, 9) ~ NA,
           T ~ NA)) %>% 
  filter(tipo_defun == 2) %>% # Homicidio (Agresión)
select(sexo, afromex, lengua, edad, escolaridad_cat, edo_nom, pareja, ent_ocurr) %>% 
  as_tibble()

# Análisis exploratorio


# Se fallece más con pareja o sin pareja?

pareja <- hom_24 %>%
  filter(sexo %in% c("Femenino", "Masculino"),
         pareja %in% c("Con pareja", "Sin pareja")) %>%
  count(sexo, pareja) %>%       
  group_by(sexo, pareja) %>%
  mutate(pct = ifelse(sexo == "Femenino", n/3253, n/25628)) %>%  
  ggplot(aes(x = pareja, y = pct, fill = sexo)) +
  geom_col(position = "identity", alpha = 0.8) +
  facet_wrap(~sexo) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.65)) +
  labs(
    y = "Proporción del total",
    x = "",
    title = "Distribución de homicidios por sexo",
    subtitle = "Porcentaje distribuido entre quienes tenían pareja y quienes no"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(filename = "03_output/pareja.png",
       plot = pareja,
       width = 10, 
       height = 6,
       dpi = 300)


# Edad a la que más se fallece por sexo

pond <- hom_24 %>%
  filter(sexo %in% c("Femenino", "Masculino")) %>%
  count(sexo, edad) %>%       
  group_by(sexo) %>%
  mutate(pct = n / sum(n)) %>%  
  ggplot(aes(x = edad, y = pct, fill = sexo)) +
  geom_col(position = "identity", alpha = 0.7) +
  facet_wrap(~sexo) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.05)) +
  labs(y = "Proporción del total",
    x = "Edad",
    title = "Distribución porcentual de homicidios por edad y sexo",
    subtitle = "") +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(filename = "03_output/dist_edad_sexo.png",
       plot = pond,
       width = 10, 
       height = 6,
       dpi = 300)


# Guanajuato es donde se concentra la mayor cantidad de homicidios

edo_sf <- st_read("02_input/00ent.shp") %>% 
  rename(ent_ocurr = CVE_ENT)

resumen_edos <- hom_24 %>%
  filter(sexo %in% c("Femenino", "Masculino")) %>%
  count(ent_ocurr, sexo, name = "num")%>%
  complete(ent_ocurr = edo_sf$ent_ocurr,
    sexo = c("Femenino", "Masculino"),
    fill = list(num = 0))%>%
  group_by(sexo) %>%
  mutate(pct_sexo = 100 * num / sum(num)) %>%
  ungroup()

mapa <- edo_sf %>%
  left_join(resumen_edos, by = "ent_ocurr")

lim_sup <- max(mapa$pct_sexo, na.rm = TRUE)

mapa_comp <- ggplot(mapa) +
  geom_sf(aes(fill = pct_sexo), color = "grey40", linewidth = 0.2) +
  facet_wrap(~sexo) +
  scale_fill_viridis_c(name = "% del total nacional\n(por sexo)",
    limits = c(0, lim_sup),
    oob = scales::squish,
    labels = function(x) paste0(round(x, 1), "%")) +
  labs(title = "Distribución porcentual de homicidios por entidad federativa y sexo",
       subtitle = "") +
  theme_void()

ggsave(filename = "03_output/mapas.png",
  plot = mapa_comp,
  width = 10, 
  height = 6,
  dpi = 300)


# Escolaridades de homicidio por lengua, sexo y afro
  
educ <- hom_24 %>% 
  pivot_longer(c(lengua, afromex, sexo),
    names_to = "categoría",
    values_to = "valor") %>% 
  filter(!is.na(valor)) %>% 
  count(categoría, valor, escolaridad_cat, name = "num") %>% 
  group_by(categoría, valor) %>% 
  mutate(pct = num / sum(num)) %>% 
  ungroup()


educ %>% 
  filter(valor %in% c("Afromexicanx", "Femenino", "Indigena", "Masculino")) %>% 
  ggplot(aes(x = reorder(escolaridad_cat, -pct), y = pct)) + 
  geom_col(fill = "Steelblue") +
  facet_wrap(~valor) +
  scale_y_continuous(labels = scales::percent_format(),
                     ) +
  labs(x = "",
       y = "Porcentaje dentro de la categoría",
       title = "Distribución de escolaridad por categoría",
       subtitle = "Porcentaje respecto al total de cada grupo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# Fuente
tema_garamond <- theme_minimal(base_family = "garamond") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(),
    strip.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )




