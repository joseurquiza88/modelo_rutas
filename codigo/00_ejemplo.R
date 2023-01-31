#######################################################################
# Prueba de la funcion:
# ------------             ESTIMACION DE EXPOSICION FINAL V02     -------------   
# VARIABLES   
# ------------01. Key tom-tom
# CONTRASEÑAS VARIAS CUENTAS DE TOM TOM. 2500 Request por dia cada una
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key=key_1

# ------------02. Modos de transporte
modo = c("Auto","Auto","Auto")

# ------------03. Lista con Coordenadas de origen-Destino

lista_viaje <- data.frame(long =c(-68.789,-68.864,-68.789),
                          lat = c(-32.88204,-32.91051,-32.87568))

# ------------04. Grilla de contaminacion local. 
# Contiene las grillas de PM de la salida de CALPUFF.
# Fijarse la direccion local de las grillas!!!!!!!!!!!!!!!!!!!!
directorio_grilla <- "D:/Josefina/Proyectos/salud/movilidad_7/grillas"

# ------------05. Tipo de ruta seleccionada
seleccion <- c("Menos contaminada","Mas rapida","Mas corta")

## ------------07. Horas de Salida de los sitios de interes.
#Ojo el formato de la fecha

horas_interes<- c("2023-01-23 07:50:00 -03","2023-01-23 11:10:00 -03",
                  "2023-01-23 14:30:00 -03")



# ------------07 Corremos la funcion global
# Prueba para la salida en formato de plot
prueba_plot <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grilla,key,
                           seleccion,salida_exp="plot",horario = horas_interes)
# Prueba para la salida en formato de df
prueba_df <- exposicion_total(lista_viaje,tiempo_actividad, modo, concentraciones_grilla=directorio_grilla,key,
                              seleccion,salida_exp="df",horario = horas_interes)

# ------------08 Guardamos carpeta. Fijarse el path
#Guardar en en un cv
write.csv(prueba_df,"prueba_df.csv")


# Guardar en en un html
htmlwidgets::saveWidget(prueba_plot, "prueba_plot.html")
