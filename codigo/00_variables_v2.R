#################################################################

# Ejemplo 02 de modelo de exposicion basado en la movilidad y actividades
# 0. Correr librerias necesarias para el modelo. Sino estan instaladas
# instalar. Revisar cuales son  importantes!!
# 00_library

#1.  Correr las funciones de los archivos .R:
          # 01_recorridos_request.R
          # 02_funcion_point-to-line.R
          # 03_horas_v2.R
          # 04_exposicion_V04.R
          # 06_seleccion_puntos.R

#2. Ubicarse en el path donde se encuentran las grillas de CALPUFF
setwd("D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp")
#3. Variables

# ------------01. Puntos de origen-destino
distritos <- "D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/shape/distritos_interes.shp"
grilla <- "D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/grilla/grilla_utm.shp"
puntosOD <- seleccion_puntos_poblacion(distritos,grilla)
lista_viaje <- data.frame(long =c(puntosOD$x[1],puntosOD$x[2],puntosOD$x[3]),
                          lat = c(puntosOD$y[1],puntosOD$y[2],puntosOD$y[3]))

  
# ------------02. Key tom-tom
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1

# ------------03. Modos de transporte 
# Siempre considerar el ida - vuelta
modo = c("Auto","Auto","Colectivo")

# ------------04. Directorio con Grilla CALPUFF
# las grillas estan por dia-hora
directorio_grillas <- "D:/Josefina/Proyectos/CALPUFF/Resultados/PM25/temp"
setwd(directorio_grillas)

# ------------05. Tipo de ruta seleccionada
seleccion <- c("Mas rapida","Mas rapida","Menos contaminada")

## ------------07. Horas de Salida del hogar por primera vez. Dato aleatorio
dia <- "2018-08-05"
# Funcion que Trae datos aleatorios del horario_salida_hogar, tipo_transp,tiempo_act y las actividades a realizar)
df_datos_aleatorios <- datos_aleatorios(1)
# solo tomamos la salida del hogar por primera vez
horario_salida_hogar<- paste(dia," ",funcion_horas (as.numeric(df_datos_aleatorios[1])),":00 -03",sep = "")
## ------------08. Tiempo de duracion de cada actividad. Dato aleatorio
minutosActividades<-tiempo_act_aleatorios(num_actividad=2)


# Pruebas varias
prueba_df <- exposicion_total(lista_viaje, modo, concentraciones_grilla=directorio_grillas ,key,
                              seleccion,salida_exp="df",horario_salida_hogar, minutosActividades)


prueba_plot <- exposicion_total(lista_viaje, modo, concentraciones_grilla=directorio_grillas ,key,
                              seleccion,salida_exp="plot",horario_salida_hogar, minutosActividades)

# ------------ Guardamos ejemplos
# Path local
setwd("D:/Josefina/Proyectos/salud/movilidad_7/ejemplos_varios")
# Salida DF
write.csv(prueba_df,"./prueba_df.csv")
# Salida plot
htmlwidgets::saveWidget(prueba_plot, "prueba_plot.html")
