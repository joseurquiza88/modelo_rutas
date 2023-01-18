#######################################################################
# ------------        ESTIMACION DE LA EXPOSICION TOTAL     -------------     

#Este codigo permite estimar la exposicion total diaria
# La idea es ingresar una lista () de coordenadas con los puntos de O-D
# El ultimo viaje siempre sera el hogar, es decir el primer punto ingresado
#

exposicion_total <- function (lista_viaje,tiempo_actividad, modo, grilla,key,seleccion,salida_exp){
  df_salida <- data.frame()
  rbind_df_1 <- data.frame()
  grilla <- st_read(concentraciones_grilla)
  rbind_ruta_selec <- data.frame()
  # ------- Calculo para mas de un viaje diario
  for (i in 1:length(lista_viaje$long)){
    print(i)
    # ------- Si las coordenadas corresponden al ultimo punto ingresado
    # El destino vuelve al punto 1 (hogar)
    if (i == length(lista_viaje$long)){
      origen_coords <- paste((lista_viaje[i,2]),(lista_viaje[i,1]),sep = ",")
      origen_coords_1 <- paste((lista_viaje[1,2]),(lista_viaje[1,1]),sep = ",")
      ruta_seleccionada <-  alternativas_recorridos (origen=origen_coords,destino=origen_coords_1,modo=modo[i], concentraciones_grilla,key=key_1,salida = "df")
      
      }else {
        origen_coords <- paste((lista_viaje[i,2]),(lista_viaje[i,1]),sep = ",")
        destino_coords <- paste((lista_viaje[i+1,2]),(lista_viaje[i+1,1]),sep = ",")
        
        ruta_seleccionada <-  alternativas_recorridos (origen=origen_coords,destino=destino_coords,modo=modo[i], concentraciones_grilla,key=key_1,salida = "df")
        
      }
    ruta_seleccionada <- ruta_seleccionada[ruta_seleccionada$tipo == seleccion[i],]
    ruta_seleccionada$i <- paste ("Ruta",i, sep = " ") 
    # ------- Datos destino
    datos_destino <-  ruta_seleccionada[ruta_seleccionada$ID == (length(ruta_seleccionada$ID)),]
    lat_destino <- datos_destino$lat
    long_destino <- datos_destino$long

    tiempo_destino <- tiempo_actividad[i]#ingresado por el usuario en mins
    # ------- Datos trayecto IDA
    # en mins, tiempo actualconsiderando la hora IDA
    tiempo_trayecto <- ruta_seleccionada$travelTimeInSeconds[1]
    distancia_trayecto <- ruta_seleccionada$lengthInMeters[1]
    conc_trayecto <- mean (ruta_seleccionada$PMDIARIO, na.rm = T) 
    # ------- Datos origen
    datos_origen <-  ruta_seleccionada[ruta_seleccionada$ID == 1,]
    lat_origen <- datos_origen$lat
    long_origen <- datos_origen$long
    
    # ------- Datos de concentraciones de O-D puntos
    df_concentraciones <- rbind(datos_origen,datos_destino)
    coordinates(df_concentraciones) <- ~long+lat
    proj4string(df_concentraciones) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    #Guardamos la informacion en un .shp temporal
    writeOGR(df_concentraciones,".","temp_punto", driver="ESRI Shapefile")
    punto <- st_read("temp_punto.shp")
    interseccion_punto <- st_intersection(punto,grilla)
    conc_origen<- interseccion_punto[interseccion_punto$PMDIARIO == interseccion_punto$PMDIARIO[interseccion_punto$ID == min(interseccion_punto$ID)],]
    conc_destino<- interseccion_punto[interseccion_punto$PMDIARIO == interseccion_punto$PMDIARIO[interseccion_punto$ID == max(interseccion_punto$ID)],]
    conc_destino <- conc_destino$PMDIARIO
    conc_origen <- conc_origen$PMDIARIO
    print("Archivo eliminado OK")
    file.remove(file.path("./", dir(path="./" ,pattern="temp_punto.*")))
    
    df_1 <- data.frame(lat_origen,long_origen, lat_destino, long_destino,
                     conc_origen, conc_trayecto, conc_destino,
                     tiempo_trayecto,tiempo_destino,i)
    rbind_df_1<- rbind(rbind_df_1, df_1)
    
    # ---- Si queremos un plot 
    rbind_ruta_selec<- rbind(rbind_ruta_selec,ruta_seleccionada)

    
  }
  # ------- Variables finales
  tiempo_tot <- sum(rbind_df_1$tiempo_trayecto + rbind_df_1$tiempo_destino)
  tiempo_origen <- 1440 - tiempo_tot# lo calculamos al final, en mins/ 60
  rbind_df_1$tiempo_origen <-NA
  rbind_df_1$tiempo_origen[length(  rbind_df_1$lat_origen)] <-  tiempo_origen
  rbind_df_1$tiempo_destino[length(  rbind_df_1$lat_origen)] <-  NA
  #Exposicion total trayecto
  rbind_df_1$exp_tot_trayecto <- rbind_df_1$conc_trayecto *  rbind_df_1$tiempo_trayecto
  rbind_df_1$exp_tot_destino <-   rbind_df_1$conc_destino *   rbind_df_1$tiempo_destino
  rbind_df_1$exp_tot_origen <-   rbind_df_1$conc_origen *   rbind_df_1$tiempo_origen
  
  # ------- Calculamos exposiciones por hora  mins
  exp_origen <- mean (rbind_df_1$exp_tot_origen, na.rm=T)
  exp_trayecto <- mean (rbind_df_1$exp_tot_trayecto, na.rm=T)
  exp_destino <- mean (rbind_df_1$exp_tot_destino, na.rm=T)
  exp_tot <- round((mean(c(exp_origen,exp_destino,exp_trayecto), na.rm=T)/60),2)
  rbind_df_1$exp_tot <- exp_tot
  
  rbind_df_1$ruta <- paste("Ruta",rbind_df_1$i,sep=" " )
  tiempo_origen_function <- funcion_horas(tiempo_origen )
  tiempo_trayecto_function <- funcion_horas((sum(rbind_df_1$tiempo_trayecto)))
  tiempo_destino_function <- funcion_horas(sum(tiempo_actividad))
  
  #  --- Transformacion de puntos a lineas
  ruta_line<- points_to_line(data = rbind_ruta_selec, 
                                               long = "long", 
                                               lat = "lat", 
                                               id_field = "i",
                                               sort_field = "ID")
  #  --- Caracteristicas en HTML del titulo
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 10px;
  }
"))
  #  --- Titulo del mapa
  title <- tags$div(tag.map.title, HTML(paste(sep = "<br/>", 
                               paste0("<center><b>Estimacion de la exposicion total diaria </b></center>"),
                               paste0("<b>Exposicion: </b>",  exp_tot ," µg m-3/h"),
                               paste0("<b>Tiempo en origen: </b>",  tiempo_origen_function,"hs"),
                               paste0("<b>Tiempo en actividades: </b>",  tiempo_destino_function ," hs"),
                               paste0("<b>Tiempo en viaje: </b>",  tiempo_trayecto_function ," hs"))))
  
  #  --- ID lista viaje
  num_rows<-  nrow(lista_viaje)
  id <- c(1:num_rows)
  lista_viaje<- cbind(id , lista_viaje)

  #  --- Categorias grilla
  grilla$categorias = case_when(grilla$PMDIARIO<=12.1 ~ 'Bueno',
                                grilla$PMDIARIO>12.1 & grilla$PMDIARIO <= 35.4  ~ 'Moderado',
                                grilla$PMDIARIO >35.4 & grilla$PMDIARIO <= 55.4  ~ 'Insalubre para personas sensibles',
                                grilla$PMDIARIO > 55.4 & grilla$PMDIARIO <= 150.4  ~ 'Insalubre',
                                grilla$PMDIARIO > 150.4 & grilla$PMDIARIO <= 250.4  ~ 'Muy Insalubre',
                                grilla$PMDIARIO > 250.4 ~ 'Peligroso' )

  #  --- Colores grilla
  paleta_grilla <- c("#abdda4","#f8fd66","#fdde61","#d74a4c","#b687ba","#590e63")
  paleta_ruta <- c("#023858","#49006a","#00441b","#e7298a","#feb24c","#3690c0","#016c59","#8c510a","#f03b20")
  palfac <- colorFactor(paleta_grilla, domain = grilla$categorias)
  pal <- colorFactor(paleta_ruta, domain = rbind_df_1$i)
  # ---  Plot 
  mapa <- leaflet() %>%
    addTiles() %>%
    
    addAwesomeMarkers(lista_viaje,lng=lista_viaje$long,lat =lista_viaje$lat,label = paste("Punto",lista_viaje$id,sep = " ")) %>%
    addPolylines(data = ruta_line,stroke = TRUE,opacity = 0.8,weight = c((rbind_df_1$i)+3), color = c(pal(rbind_df_1$i)),group = (as.character(rbind_df_1$ruta))) %>%
    addPolygons(data = grilla,color = "#636363" ,
                group = "Concentraciones",
                weight = 2,
                smoothFactor = 0.1,
                opacity = 0.1,
                fillOpacity = 0.5,
                fillColor = ~palfac(grilla$categorias))%>%
    addTiles() %>%
    addControl(title, position = "topleft", className="map-title")%>%
    addLegend(data = grilla,position = "bottomleft", pal = palfac, values = ~grilla$categorias, 
              title = "Concentraciones PM2.5 (µg m-3)")%>%
    # Layers control
    addLayersControl(
      overlayGroups = c("Concentraciones",c(rbind_df_1$ruta)))#,"Ruta menos contaminada", "Ruta mas contaminada", "Ruta mas corta","Ruta mas rapida"))#,
  
  if (salida_exp == "df"){
    return(rbind_df_1)
  }
  if (salida_exp == "plot"){
    return(mapa)
    
  } 
}



