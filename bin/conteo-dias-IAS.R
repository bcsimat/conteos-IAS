# julio 2023
# codificacion UTF-8

# limpiar el ambiente de trabajo
rm(list=ls())
gc(reset=T)

Sys.getlocale()
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")

# Elegir el directorio donde se encuentra el script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Cargar librerias
#if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(readxl, tidyverse, magrittr, # manipulacion de tablas
       lubridate, # manipulacion de fechas
       showtext, # fuentes
       geomtextpath, patchwork, Cairo) # coord_curvedpolar() y graficos

#  datos ----
# Cargar tablas de datos
datos.h <- read_csv(file = gzfile(description = "../data/contaminantes_2022.CSV.gz"), skip = 10)
glimpse(datos.h)
unique(datos.h$id_parameter)

pms.nc <- read_csv(file = gzfile(description = "../data/particulas_nowcast_2022.csv.gz"))

# Estaciones consideradas de "transporte" de contaminantes
transporte <- c("ACO", "AJU", "INN", "MON", "MPA")

# seleccionar variables, transformar datos y categorizar por intervalos
# Ozono concentracion horaria, obtener maximo del promedio por dia
o31h <- datos.h %>% filter(id_parameter == "O3") %>% rename(valor = value)
o31h$date <- as.POSIXct(strptime(o31h$date, format = "%d/%m/%Y %H", tz = "GMT"))
o31h %<>% mutate(anio = year(date)) %>% filter(anio == 2022)

o3.1h <- o31h %>% filter(!id_station %in% transporte) %>% # se eliminan estaciones transporte
  mutate(fecha = date(date), hora = hour(date), .after = date) %>%
  group_by(fecha) %>%
  summarise(maximo = max(valor, na.rm = T)) %>%
  mutate(maximo = maximo/1000, # unidades en ppb, convertir a ppm
         # Intervalos para obtener el índice para ozono 1h NOM-172-SEMARNAT-2019
         o31h_cat = case_when(maximo <= 0.051 ~ 1,
                          maximo > 0.051 & maximo <= 0.095 ~ 2,
                          maximo > 0.095 & maximo <= 0.135 ~ 3,
                          maximo > 0.135 & maximo <= 0.175 ~ 4,
                          maximo > 0.175 ~ 5, TRUE ~ 0)) %>% ungroup()

# promedio movil ponderado de 12h de Particulas Menores a 10 y 2.5 um
pms.nc2 <- pms.nc %>% filter(!id_station %in% transporte) %>%
  mutate(fecha = date(date), hora  = hour(date), .after = date,
         valor = as.numeric(valor)) %>% group_by(fecha, id_parameter) %>%
  summarise(maximo = max(valor, na.rm = T)) %>%
  pivot_wider(names_from = id_parameter, values_from = maximo) %>%
  mutate(pm10_cat = case_when(PM10 %in% 0:50 ~ 1,
                              PM10 %in% 51:75 ~ 2,
                              PM10 %in% 76:155 ~ 3,
                              PM10 %in% 156:235 ~ 4,
                              PM10 >= 236 ~ 5, TRUE ~ 0),
         # Intervalos para obtener el índice para particulas NOM-172-SEMARNAT-2019
         pm25_cat = case_when(PM2.5 %in% 0:25 ~ 1,
                              PM2.5 %in% 26:45 ~ 2,
                              PM2.5 %in% 46:79 ~ 3,
                              PM2.5 %in% 80:147 ~ 4,
                              PM2.5 >= 148 ~ 5, TRUE ~ 0)) %>% ungroup()

# Asignar días con el valor maximo de los contaminantes
# funcion de categorizacion del Indice de Aire y Salud
ias <- function(categ) {
  ias = ifelse(categ == 1, "Buena",
               ifelse(categ == 2, "Aceptable", 
                      ifelse(categ == 3, "Mala",
                             ifelse(categ == 4, "Muy Mala",
                                    ifelse(categ == 5, "Ext. Mala", NA_real_)))))
  return(factor(ias,
                levels = c("Buena", "Aceptable", "Mala", "Muy Mala", "Ext. Mala"),
                ordered = T))
}

cat.ias <- left_join(o3.1h, pms.nc2, by = "fecha") %>%
  mutate(mes = month(fecha, label = T),
         parametro = "todos", .after = fecha) %>%
  # obtener valor máximo de categoría por día (fila) de todos los contaminantes
  rowwise() %>% mutate(dia_cat = max(c_across(matches("_cat")), na.rm = T),
                       indice = ias(categ = dia_cat)) %>% # categorizar
  ungroup()

# Contar categorias del indice por mes de cada contaminante
cat.x.mes <- list(o31h = o3.1h %>% select(fecha, o31h_cat),
               pm10 = pms.nc2 %>% select(fecha, pm10_cat),
               pm25 = pms.nc2 %>% select(fecha, pm25_cat)) %>%
  lapply(., function(x)
  x %>% pivot_longer(cols = 2, names_to = "parametro", values_to = "valor") %>%
    mutate(mes = month(fecha, label = T),
           indice = ias(valor), .after = fecha) %>%
    group_by(parametro, mes, indice) %>% tally() %>% ungroup())
cat.x.mes$todos = cat.ias %>% group_by(parametro, mes, indice) %>% tally() %>% ungroup()

# graficos ----
# Colores grises en graficas
grisosc = "#6F7271"
grisclaro = "#DFDFDF"

# fuentes 
# desde google
font_add_google(name = "Quicksand", family = "qsand")
# desde archivo de PC
#font_add(family = "qsand", regular = "../data/Quicksand/static/Quicksand-Regular.ttf",
 #        bold = "../data/Quicksand/static/Quicksand-Bold.ttf")

showtext_auto()
mifont = "qsand"

## Barras ----
ias.plots <- function(d) {
  p1 <- ggplot(data = d, aes(x = mes, y =  n)) +
    geom_col(aes(fill = indice),# alpha = 0.5, 
             position = position_dodge(width = 1)) +
    geom_vline(xintercept = 1:13 - 0.5, color = grisclaro, size = 0.8) +
    geom_hline(yintercept = -1:6 * 5, color = grisclaro, size = 0.5, alpha = 0.5) +
    scale_fill_manual(values = c("Buena" = "#00e400", # colores oficiales del IAS
                                 "Aceptable" = "#ffff00",
                                 "Mala" = "#ff7e00",
                                 "Muy Mala" = "#ff0000",
                                 "Ext. Mala" = "#8f3f97")) +
    scale_y_continuous(name = "Número de días", breaks = 0:6 *5) +
    theme_bw(base_family = mifont, base_size = 24) +
    coord_curvedpolar() # grafico circular
  
  # Obtener los breaks del eje y
  brk <- ggplot_build(p1)$layout$panel_params[[1]]$r.major
  brk <- brk[-c(1, length(brk))] # quitar los extremos
  
  g <- p1 +
    # escribir los breaks dentro del plot
    annotate('text', x = 0.5, y = brk, label = as.character(brk),
             size = 8, color = grisosc) +
    theme(panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 28, colour=grisosc),
          legend.title = element_blank(),
          legend.text = element_text(colour = grisosc, size = 26),
          legend.position = "left",
          panel.grid.major = element_blank(),
          axis.title.y = element_text(face = "bold", colour=grisosc),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(g)
}

barras.plot <- lapply(X = cat.x.mes, FUN = ias.plots)

## Mosaico ----
# se hace con los valores del Indice de Calidad del Aire y Riesgos a la Salud
# NOM-172-SEMARNAT-2019
mosaico.dat <- list(o3 = o3.1h[,-2], pm10 = pms.nc2[, c("fecha", "pm10_cat")],
                    pm25 = pms.nc2[, c("fecha", "pm25_cat")],
                    todos = cat.ias[, c("fecha", "dia_cat")]) %>%
  lapply(., function(x)
    x %>% rename(indice = 2) %>% mutate(ias = ias(indice)) %>%
      mutate(mes = month(fecha, label = T), dia = day(fecha)))

mosaico.plot <- mosaico.dat %>% lapply(., function(x)
  ggplot(data = x) +
    geom_tile(aes(x = mes, y = dia, fill = ias),
              color = "white", lwd = 1, linetype = 1) +
    scale_fill_manual(values = c("Buena" = "#00e400", # colores oficiales del IAS
                                 "Aceptable" = "#ffff00",
                                 "Mala" = "#ff7e00",
                                 "Muy Mala" = "#ff0000",
                                 "Ext. Mala" = "#8f3f97")) +
    scale_y_continuous(name = "", trans = "reverse",
                       breaks = unique(x$dia), expand = c(0,0)) +
    scale_x_discrete(name = "", position = "top") +
    theme_bw(base_family = mifont, base_size = 22) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 24, face = "bold", colour = grisosc),
          axis.text.y = element_text(size = 22, colour = grisosc),
          legend.position = "none")
    )


# guardar ----
# plots en formato png y svg y tabla en csv
## O3 ----
o3 <- (mosaico.plot$o3 + barras.plot$o31h) +
  plot_annotation(title = "Índice de Calidad del Aire y Riesgos a la Salud",
                  subtitle = "Máximo de concentraciones horarias de Ozono (O\u2083, ppb)",
                  theme = theme(plot.title = element_text(family = mifont, face = "bold",
                                                          colour = grisosc,
                                                          size = 30, hjust = 0.5),
                                plot.subtitle = element_text(family = mifont,
                                                             colour = grisosc,
                                                             size = 30, hjust = 0.5)))
Cairo(file="../figs/O31h_conteo.png", type="png", dpi=120, units="px", 
      width=2000, height=1200,  bg = 'white')
o3
dev.off()

Cairo(file="../figs/O31h_conteo.svg", type="svg", dpi=100, units="px", 
      width=2200, height=1500,  bg = 'white')
o3
dev.off()

cat.x.mes$o31h %>% dplyr::rename(conteo_dias = n) %>%
  write.csv(file="../data/O31h_conteo.csv", row.names = F)

## PM10 ----
pm10 <- (mosaico.plot$pm10 + barras.plot$pm10) +
  plot_annotation(title = "Índice de Calidad del Aire y Riesgos a la Salud",
                  subtitle = "Máximo del promedio ponderado 12h de PM\u2081\u2080 (µg/m\u00b3)",
                  theme = theme(plot.title = element_text(family = mifont, face = "bold",
                                                          colour = grisosc,
                                                          size = 30, hjust = 0.5),
                                plot.subtitle = element_text(family = mifont,
                                                             colour = grisosc,
                                                             size = 30, hjust = 0.5)))

Cairo(file="../figs/PM10_conteo.png", type="png", dpi=120, units="px", 
      width=2000, height=1200,  bg = 'white')
pm10
dev.off()

Cairo(file="../figs/PM10_conteo.svg", type="svg", dpi=100, units="px", 
      width=2200, height=1500,  bg = 'white')
pm10
dev.off()

cat.x.mes$pm10 %>% dplyr::rename(conteo_dias = n) %>%
  write.csv(file="../data/PM10_conteo.csv", row.names = F)

## PM25 ----
pm25 <- (mosaico.plot$pm25 + barras.plot$pm25) +
  plot_annotation(title = "Índice de Calidad del Aire y Riesgos a la Salud",
                  subtitle = "Máximo del promedio ponderado 12h de PM\u2082.\u2085 (µg/m\u00b3)",
                  theme = theme(plot.title = element_text(family = mifont, face = "bold",
                                                          colour = grisosc,
                                                          size = 30, hjust = 0.5),
                                plot.subtitle = element_text(family = mifont,
                                                             colour = grisosc,
                                                             size = 30, hjust = 0.5)))
Cairo(file="../figs/PM25_conteo.png", type="png", dpi=120, units="px", 
      width=2000, height=1200,  bg = 'white')
pm25
dev.off()

Cairo(file="../figs/PM25_conteo.svg", type="svg", dpi=100, units="px", 
      width=2200, height=1500,  bg = 'white')
pm25
dev.off()

cat.x.mes$pm25 %>% dplyr::rename(conteo_dias = n) %>%
  write.csv(file="../data/PM25_conteo.csv", row.names = F)

## todos ----
todos <- (mosaico.plot$todos + barras.plot$todos) +
  plot_annotation(title = "Índice de Calidad del Aire y Riesgos a la Salud",
                  subtitle = "Ozono y Partículas Menores a 10 y 2.5 µm",
                  theme = theme(plot.title = element_text(family = mifont, face = "bold",
                                                          colour = grisosc,
                                                          size = 30, hjust = 0.5),
                                plot.subtitle = element_text(family = mifont,
                                                             colour = grisosc,
                                                             size = 30, hjust = 0.5)))
Cairo(file="../figs/todos_conteo_v3.png", type="png", dpi=120, units="px", 
      width=2000, height=1200,  bg = 'white')
todos
dev.off()

Cairo(file="../figs/todos_conteo_v3.svg", type="svg", dpi=100, units="px", 
      width=2200, height=1500,  bg = 'white')
todos
dev.off()

cat.x.mes$todos %>% dplyr::rename(conteo_dias = n) %>%
  write.csv(file="../data/todos_conteo.csv", row.names = F)

### fin

