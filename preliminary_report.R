#!/usr/bin/env Rscript
#########################################################################################
#
# Severe Wather Reports by citizens for RELAMPAGO Proyect
# PRELIMINARY Report from 09Z to 09Z every day ploted at 08Z
# 
#########################################################################################

# Libraries
library(googlesheets)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggforce)
library(dplyr)
library(metR)


###### Reading the data from Google Spreadsheet
#gs_ls() #Return a tibble with google sheets available

r <- gs_title("Reportes_preprocesados")
reports <- setDF(gs_read(ss = r, ws = "Hoja 1"))

reports_s <- reports[, c(7,11,15,20,25,40,41,54,58,61,62)]
colnames(reports_s) <- c("date", "var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8", "lat", "lon")
reports_s$var2 <- ifelse(reports_s$var2 == "Sí", "Inundaciones/Crecidas", NA)


reports_s$date <- force_tz(as.POSIXct(as_datetime(reports_s$date, format = "%m/%d/%Y %H:%M:%S")), 
                           "America/Buenos_Aires")
hour(reports_s$date) <- hour(reports_s$date) + 3
reports_s$date <- force_tz(reports_s$date, tzone = "UTC") #Convierto a UTC posta

# Long format
reports_l <- melt(reports_s, id.vars = c("date", "lat", "lon"))
reports_l$value <- ifelse(reports_l$value == "Lluvia intensa", "Lluvias intensas", reports_l$value)
reports_l$value <- ifelse(reports_l$value == "No observé ningún otro evento", NA, reports_l$value)
reports_l$value <- ifelse(reports_l$value == "Ninguno de los anteriores", NA, reports_l$value)
reports_l$value <- ifelse(reports_l$value == "Tornado, tromba, remolino de polvo y/o nube embudo", NA, reports_l$value)
reports_l$value <- ifelse(reports_l$value %in% c("Tromba", "Torbellino de Polvo", "Nube Tubular (Funnel)"), NA, reports_l$value)

levels <- c("Granizo", "Inundaciones/Crecidas", "Lluvias intensas", 
            "Ráfagas y/o vientos intensos", "Rayos y/o truenos", "Tornado" )
labels <- c("Hail", "Floods", "Heavy rain", "Strong wind/gust", "Lightning", "Tornado")

reports_l$value <- as.factor(reports_l$value)

# Translate the factor levels
levels(reports_l$value) <- labels

##### Ploting

mapa.ar <- setDT(fortify(rnaturalearth::ne_states(country = c("Argentina"))))
setnames(mapa.ar, "long", "lon")

day <- Sys.Date()

end_time <- as_datetime(paste0(as.character(day), " 09:00:00"))
print(end_time)
init_time <- end_time
day(init_time) <- day(end_time) - 1
title <- paste0("PRELIMINARY SEVERE WEATHER REPORTS\nfrom ", init_time, "Z to ", end_time, "Z")

df <- subset(reports_l, !is.na(value) & date %between% c(init_time, end_time))
df$int <- interval(init_time, df$date)

freq <-  as.data.table(table(df$value))  %>% 
  .[, shape := c(15, 16, 17, 0, 4, 2)] %>% 
  .[N != 0] 
freq[, label := paste0(freq$V1, " (", freq$N, ")")]


ggplot(df, aes(lon, lat)) +
  geom_point(aes(color = as.numeric(int)/3600, shape = value), alpha = 0.8, size = 4) +
  geom_path(data = mapa.ar, aes(x = lon, group = group), color = "black", size = 0.2) +
  scale_color_viridis_c(name = "Hours from 09Z", option = "plasma", direction = -1,
                        limits = c(0, 24),
                        guide = guide_colorstrip(title.position = "top",
                                                 title.theme = element_text(size = 10),
                                                 barheight = 0.5,
                                                 label.theme = element_text(size = 8),
                                                 label.vjust = 0.8),
                        breaks = MakeBreaks(4)) +
  scale_shape_manual(name = NULL, values = freq$shape, labels = freq$label) +
  scale_x_longitude(name = "Lon", limits = c(-71, -58), expand = c(0,0), ticks = 2.5) +
  scale_y_latitude(name = "Lat", limits = c(-37, -28), expand = c(0,0), ticks = 2) +
  ggtitle(title) +
  coord_map() +
  theme_linedraw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom")

filename <- paste0("fig/PRELIMINARY_severe_weather_reports_", format(end_time, "%Y%m%d%H%M%S"), ".png")
ggsave(filename, dpi = 300, height = 15, units = "cm")
