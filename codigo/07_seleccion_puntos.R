#Seleccion de origen - destino
seleccion_origen_destino <- function (distrito_origen,distrito_destino,grilla,distritos){
#Abrir grilla
grilla_shp <- st_read(grilla)
grilla_transform <-st_transform(grilla_shp, crs=32519)

#Interseccion con distritos
distritos_shp<- st_read(distritos)
interseccion_grilla <- st_intersection(distritos_shp,grilla_transform)
centroide_grilla <- st_centroid(interseccion_grilla )
# Eleccion distrito destino - origen
seleccion_distrito_origen <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_origen,]
seleccion_distrito_destino <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_destino,]

# Seleccion del punto
rbind_df <- data.frame()
for (x in 1:5) {
  punto_origen <-seleccion_distrito_origen[seleccion_distrito_origen$GRI1_ID == (sample(seleccion_distrito_origen$GRI1_ID,1)),]
  punto_destino <-seleccion_distrito_destino[seleccion_distrito_destino$GRI1_ID == (sample(seleccion_distrito_destino$GRI1_ID,1)),]
  df <- data.frame(origen_x = punto_origen$X_COORD, origen_y = punto_origen$Y_COORD,
                   destino_x = punto_destino$X_COORD, destino_y = punto_destino$Y_COORD)
  rbind_df <- rbind(rbind_df,df)
  }

return(rbind_df)
}
p<-seleccion_origen_destino (distrito_origen= "Ciudad de Godoy Cruz",distrito_destino="Villa Nueva",grilla,distritos)
distritos<- "D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/shape/distritos_interes.shp"
grilla <- "D:/Josefina/Proyectos/salud/movilidad_7/grillas/grilla_00.shp"
