library(ggplot2)
library(readxl)
library(textshape)
library(tidyverse)
library(tidyr)
library(pander)

llamados <- read_excel("/Users/ceciliaguillametchargue/Desktop/COVID_19/llamados_107_covid-4.xlsx")
llamados$fecha_llamado <- as.Date(llamados$fecha_llamado, format = "%d/%m/%Y")

##################### Llamados 107 ########################

llamados2 <- llamados %>%
                group_by(fecha_llamado) %>%
                tally()
llamados2 <- llamados2[-1,]
    
ggplot(llamados2, aes(x = fecha_llamado, y = n)) +
      geom_line(color = "darkorange", size = 1) +
      labs(x = "Fecha del llamado", y = "Frecuencia", title = "Frecuencia de llamados al 107 por día") +
      theme(plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                 vjust=1, #Justificación vertical, para separarlo del gráfico
                                 face="plain", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                 lineheight=1))



##################### Casos en TDF ########################

casos <- read_excel("/Users/ceciliaguillametchargue/Desktop/COVID_19/TIERRA_DEL_FUEGO-05-11-2020_12-47-16.xlsx")
casos$FECHA_APERTURA <- as.Date(casos$FECHA_APERTURA, format = "%d/%m/%Y")
confirmados <- casos[casos$CLASIF_RESUMEN == "Confirmado",]

confirmados2 <- confirmados %>%
  group_by(FECHA_APERTURA) %>%
  tally()

ggplot(confirmados2, aes(x = confirmados2$FECHA_APERTURA, y = confirmados2$n)) +
  geom_line(color = "darkorange", size = 1) +
  labs(x = "Fecha de notificación", y = "Frecuencia", title = "Frecuencia de casos nuevos por día") +
  theme(plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                  vjust=1, #Justificación vertical, para separarlo del gráfico
                                  face="plain", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                  lineheight=1))

##################### Casos en Ushuaia ########################

confirmadosUSH <- confirmados[confirmados$LOCALIDAD_RESIDENCIA == "USHUAIA",]
confirmadosUSH2 <- confirmadosUSH %>%
  group_by(FECHA_APERTURA) %>%
  tally()

ggplot(confirmadosUSH2, aes(x = confirmadosUSH2$FECHA_APERTURA, y = confirmadosUSH2$n)) +
  geom_line(color = "#2E86C1", size = 1) +
  labs(x = "Fecha de notificación", y = "Frecuencia", title = "Frecuencia de casos nuevos por día en Ushuaia") +
  theme(plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                  vjust=1, #Justificación vertical, para separarlo del gráfico
                                  face="plain", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                  lineheight=1))



##################### Casos en Río Grande ########################

confirmadosRG <- confirmados[confirmados$LOCALIDAD_RESIDENCIA == "RIO GRANDE",]
confirmadosRG2 <- confirmadosRG %>%
  group_by(FECHA_APERTURA) %>%
  tally()

ggplot(confirmadosRG2, aes(x = confirmadosRG2$FECHA_APERTURA, y = confirmadosRG2$n)) +
  geom_line(color = "#E67E22", size = 1) +
  labs(x = "Fecha de notificación", y = "Frecuencia", title = "Frecuencia de casos nuevos por día en Río Grande") +
  theme(plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                  vjust=1, #Justificación vertical, para separarlo del gráfico
                                  face="plain", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                  lineheight=1))


##################### Casos por localidad ########################

confirmados3 <- confirmados %>%
  group_by(FECHA_APERTURA, LOCALIDAD_RESIDENCIA) %>%
  tally()

confirmados3 <- subset(confirmados3, confirmados3$LOCALIDAD_RESIDENCIA == c("USHUAIA", "RIO GRANDE"))
table(confirmados3$LOCALIDAD_RESIDENCIA)

legend_title <- "localidad"
ggplot(confirmados3, aes(x = confirmados3$FECHA_APERTURA, y = confirmados3$n, group = confirmados3$LOCALIDAD_RESIDENCIA, 
                         colour = confirmados3$LOCALIDAD_RESIDENCIA)) +
  geom_line(size = 1) +
  scale_color_manual(name = "Localidad", values = c("#E67E22", "#2E86C1")) +
  labs(x = "Fecha de notificación", y = "Frecuencia", title = "Frecuencia de casos nuevos por día por localidad", group = "Localidad") +
  theme(plot.title = element_text(size=rel(2), #Tamaño relativo de la letra del título
                                  vjust=1, #Justificación vertical, para separarlo del gráfico
                                  face="plain", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                  lineheight=1),
                                  legend.position = "bottom")



############## Casos confirmados USH por GE ###############
confirmadosGEUSH <- confirmadosUSH %>%
  group_by(GRUPO_ETARIO_DECADA) %>%
  tally()


############## Casos confirmados RG por GE ###############
confirmadosGERG <- confirmadosRG %>%
  group_by(GRUPO_ETARIO_DECADA) %>%
  tally()


############## Defunciones USH por GE ###############

defuncionesUSH <- confirmadosUSH[!is.na(confirmadosUSH$FALLECIDO),]
defuncionesUSH <- defuncionesUSH[defuncionesUSH$FALLECIDO == "SI",]
defuncionesUSH <- defuncionesUSH %>%
  group_by(GRUPO_ETARIO_DECADA) %>%
  tally()


############## Defunciones RG por GE ###############

defuncionesRG <- confirmadosRG[!is.na(confirmadosRG$FALLECIDO),]
defuncionesRG <- defuncionesRG[defuncionesRG$FALLECIDO == "SI",]
defuncionesRG <- defuncionesRG %>%
  group_by(GRUPO_ETARIO_DECADA) %>%
  tally()


##################### Tabla de información ########################

conf_ge <- left_join(confirmadosGEUSH, confirmadosGERG, by = "GRUPO_ETARIO_DECADA")
conf_ge <- pander(rename(conf_ge, "GRUPO ETARIO" = GRUPO_ETARIO_DECADA, "USHUAIA" = n.x, "RIO GRANDE" = n.y))


def_ge <- right_join(defuncionesUSH, defuncionesRG, by = "GRUPO_ETARIO_DECADA")
def_ge <- arrange(def_ge, GRUPO_ETARIO_DECADA)
def_ge <- rename(def_ge, "GRUPO ETARIO" = GRUPO_ETARIO_DECADA, "USHUAIA" = n.x, "RIO GRANDE" = n.y)
def_ge$USHUAIA <- ifelse(is.na(def_ge$USHUAIA), 0, def_ge$USHUAIA)
pander(def_ge)
