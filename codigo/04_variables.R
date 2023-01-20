
#######################################################################
# ------------             ELECCION DE LA MEJOR RUTA     -------------   
# ------------ VARIABLES   
# ------------01. Key tom-tom
# CONTRASEÑAS VARIAS CUENTAS DE TOM TOM. 2500 Request por dia cada una
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1

# ------------02. Modos de transporte

modo = "Camion"
modo = "Colectivo"
modo = "Bicicleta"
modo = "Motocicleta"
modo = "Pie"
modo ="Auto"
#modo="car"
# ------------03. Coordenadas de origen
#Ejemplo 1
origen <-"-33.05324,-68.774"
#Ejemplo 2
origen <-"-32.79679,-68.816" # CAPDEVILLA
# ------------03. Coordenadas de destino
#Ejemplo 1
destino <- "-32.90212,-68.761"
#Ejemplo 2
destino <-"-32.86246,-68.859" #CIENEGUITA

# ------------04. Grilla de contaminacion local
concentraciones_grilla <- "D:/Josefina/Proyectos/salud/movilidad_7/grilla_contaminantes/buffer_wgs.shp"#st_read("D:/Josefina/Proyectos/salud/movilidad_7/grilla_contaminantes/buffer_wgs.shp")
# ------------05. hora de salida
horas_interes <- "2023-01-19 12:20:00 -03"

# ------------05. Corremos la funcion global
prueba_alternativas1 <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla=directorio_grilla,key=key_1,salida = "df",horario = horas_interes)
prueba_plot <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla,key=key,salida = "plot",horario = horas_interes)
prueba_poly3 <- alternativas_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla,key=key,salida = "polyline",horario = horas_interes)
prueba_1_plot <- plot_recorridos (origen=origen,destino=destino,modo="Auto", concentraciones_grilla,key=key_2)

# ------------06. Lo podemos guardar 
#Guardar en en un HTML y visualizarlo en el Chrome

setwd("D:/Josefina/Proyectos/salud/movilidad_7/")
htmlwidgets::saveWidget(prueba_plot, "alternativas_recorridos_plot.html")

#Guardar en en un shape
writeOGR(prueba_poly3,"D:/Josefina/Proyectos/salud/movilidad_7","alternativas_recorridos_polyline", driver="ESRI Shapefile")

#Guardar en en un cv
write.csv(prueba_alternativas1,"alternativas_recorridos_df.csv")



#######################################################################
# ------------             ESTIMACION DE EXPOSICION FINAL V02     -------------   
# ------------ VARIABLES   
# ------------01. Key tom-tom
# CONTRASEÑAS VARIAS CUENTAS DE TOM TOM. 2500 Request por dia cada una
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1

# ------------02. Modos de transporte
modo = c("Auto","Pie","Auto")#,"Auto")

# ------------03. Lista con Coordenadas de origen-Destino

lista_viaje <- data.frame(long =c(-68.789,-68.864,-68.789,-68.857),
                         lat = c(-32.88204,-32.91051,-32.87568,-33.03039))

# ------------04. Grilla de contaminacion local
directorio_grilla <- "D:/Josefina/Proyectos/salud/movilidad_8/grillas"

# ------------05. Tipo de ruta seleccionada
seleccion <- c("Mas rapida","Mas corta","Menos contaminada")#,"Mas rapida")

## ------------07. Horas de exposicion para los puntos de interes no para la intereseccion
horas_interes<- c("2023-01-20 07:50:00 -03","2023-01-20 13:10:00 -03","2023-01-20 16:30:00 -03")#,"2023-01-19 09:50:00 -03")



# ------------07 Corremos la funcion global
prueba <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grilla,key,
                                 seleccion,salida_exp="plot",horario = horas_interes)

#Guardar en en un cv
write.csv(df_salida,"D:/Josefina/Proyectos/salud/movilidad_7/df_salida_1.csv")

setwd("D:/Josefina/Proyectos/salud/movilidad_7/salidas_ejemplo")

# ------------08 Guardamos el html en la carpeta
htmlwidgets::saveWidget(prueba, "prueba.html")

# 