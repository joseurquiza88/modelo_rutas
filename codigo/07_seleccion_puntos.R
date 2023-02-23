seleccion_puntos <- function(distrito_origen){

##########################################################################
# -- Seleccion punto de origen
#Interseccion con distritos
  
  distritos_shp<- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/shape/distritos_interes.shp")
  uso_suelo <-st_read("D:/Josefina/Proyectos/salud/movilidad_8/shape/zona_lcz/UsoSuelo_grilla.shp")
  interseccion_grilla <- st_intersection(distritos_shp,uso_suelo)
  # Pongo puntos
  centroide_grilla <- st_centroid(interseccion_grilla)
  # --  Eleccion distrito origen
  seleccion_distrito_origen <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_origen,]
  # Ordenamos datos de origen segun la poblacion
  sel_dist_origen_order <- seleccion_distrito_origen[order(seleccion_distrito_origen$POBLXGRI), ]
  # Tomamos por ahora el 15% de los puntos con mas poblacion y 5% con menor poblacion
  p_mayor_d <- sel_dist_origen_order[(length(sel_dist_origen_order$ID)-as.integer(length(sel_dist_origen_order$ID)*0.15)):length(sel_dist_origen_order$ID),]
  p_menor_d <- sel_dist_origen_order[1:(as.integer(length(sel_dist_origen_order$ID)*0.05)),]
  merge_puntos <- rbind(p_mayor_d,p_menor_d)
  punto_origen <-merge_puntos[merge_puntos$GRI1_ID == (sample(merge_puntos$GRI1_ID,1)),][1,]
  
  
  # -- Seleccion del numero de actividades
  num_actividades <- sample(c(0,2),1)
  
  
  
  if (num_actividades == 0){
    punto_rbind <- data.frame (distrito_origen = punto_origen$DISTRITOS,
                               x_origen = punto_origen$X_COORD,
                               y_origen =punto_origen$Y_COORD,
                               distrito_destino = NA,
                               x_destino =NA,
                               y_destino = NA,
                               actividad = "hogar",
                               tiempo_act = 1440,
                               transporte = NA)
    
  }else{
  
  #########################################################################
  # -- Seleccion del tipo de actividades
  # Trabajo 01
  # Recreacion 02
  # Escuela 03
  # Otra 04
    act <- sample(c("trabajo","recreacion","otra","educacion"),num_actividades)
    
    #########################################################################
    # --   Tipo de transporte
    
    tipo_transp  <- sample(c("auto","pie","moto","colectivo","bicicleta"),num_actividades+1)
    #for (j in 1:1){
    
      #########################################################################
      # --   Tiempo aleatorio por actividades
      df_tiempo_act_rbind <- data.frame()
      for (j in 1:num_actividades){
        if (act[j] == "trabajo"){ #trabajo 4 - 9hs
          tiempo_act <- sample(c(240:540),1)
        }
        if (act[j] == "recreacion"){ #Recreacion 30 min - 2hs
          tiempo_act <- sample(c(30:120),1)
        } 
        if (act[j] == "otra"){ #Otro 30 - 60 mins
          tiempo_act <- sample(c(30:60),1)
        }
        if (act[j] == "educacion"){ #Escuela 5 - 7hs
          tiempo_act <- sample(c(300:420),1)
        }
        df_tiempo_act <- data.frame(tiempo_act,j)
        df_tiempo_act_rbind <- rbind(df_tiempo_act_rbind,df_tiempo_act)
      }
    
      
      #########################################################################
      # --   Punto de origen destino segun uso de suelo
      #Seleccion de origen - destino
      punto_rbind <- data.frame()
      for (x in 1:num_actividades){
        #  --Eleccion distrito destino segun actividad
        if (act[x] == "educacion"){
        # seleccion_distrito_destino <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_destino,]
        # sel_distrito_destino_act <- seleccion_distrito_destino [seleccion_distrito_destino$New.uso == act,]
        print("A")
        sel_distrito_destino_act <- centroide_grilla[centroide_grilla$New.uso == act,]
        punto_destino <-sel_distrito_destino_act [sel_distrito_destino_act$GRI1_ID == (sample(sel_distrito_destino_act$GRI1_ID,1)),][1,]
        }
        if (act[x] == "otros"){
          print("b")
          #seleccion_distrito_destino <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_destino,]
          punto_destino <-centroide_grilla[centroide_grilla$GRI1_ID == (sample(centroide_grilla$GRI1_ID,1)),][1,]
        }
        
        if (act[x] == "trabajo"){
          print("c")
          #seleccion_distrito_destino <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_destino,]
          sel_distrito_destino_act <- centroide_grilla[(centroide_grilla$New.uso == "agricultura" |centroide_grilla$New.uso == "comercial" 
                                                                   |centroide_grilla$New.uso == "educacion" |centroide_grilla$New.uso == "industrial"
                                                                   |centroide_grilla$New.uso == "salud"),]
          punto_destino <-sel_distrito_destino_act [sel_distrito_destino_act$GRI1_ID == (sample(sel_distrito_destino_act$GRI1_ID,1)),][1,]
        }
        
        if (act[x] == "recreacion"){
          print("d")
          #seleccion_distrito_destino <- centroide_grilla[centroide_grilla$DISTRITOS == distrito_destino,]
          sel_distrito_destino_act <- centroide_grilla[(centroide_grilla$New.uso == "comercial"  |centroide_grilla$New.uso == "parque" |
                                                          centroide_grilla$New.uso == "recreativo" | centroide_grilla$New.uso == "zona natural"),]
          punto_destino <-sel_distrito_destino_act [sel_distrito_destino_act$GRI1_ID == (sample(sel_distrito_destino_act$GRI1_ID,1)),][1,]
        }
        if(x==1){
          print("e")
        punto <- data.frame (distrito_origen = punto_origen$DISTRITOS,
                             x_origen = punto_origen$X_COORD,
                             y_origen =punto_origen$Y_COORD,
                             distrito_destino = punto_destino$DISTRITOS,
                             x_destino =punto_destino$X_COORD,
                             y_destino = punto_destino$Y_COORD,
                             actividad = act[x],
                             tiempo_act = df_tiempo_act_rbind$tiempo_act[x],
                             transporte = tipo_transp[x])
        }
          
        else{
          print("f")
          punto <- data.frame (distrito_origen = punto_rbind$distrito_destino[x-1],
                               x_origen = punto_rbind$x_destino[x-1],
                               y_origen =punto_rbind$y_destino[x-1],
                               distrito_destino = punto_destino$DISTRITOS,
                               x_destino =punto_destino$X_COORD,
                               y_destino = punto_destino$Y_COORD,
                               actividad = act[x],
                               tiempo_act = df_tiempo_act_rbind$tiempo_act[x],
                               transporte = tipo_transp[x])
        }
        punto_rbind <- rbind(punto_rbind,punto)
      }
      print("g")
      df_punto_rbind <- data.frame(distrito_origen = punto_rbind$distrito_destino[length(punto_rbind$distrito_destino)],
                                   x_origen = punto_rbind$x_destino[length(punto_rbind$distrito_destino)],
                                   y_origen = punto_rbind$y_destino[length(punto_rbind$distrito_destino)],
                                   distrito_destino = punto_rbind$distrito_origen[1],
                                   x_destino = punto_rbind$x_origen[1],
                                   y_destino = punto_rbind$y_origen[1],
                                   actividad = "hogar",
                                   tiempo_act = NA,
                                   transporte = tipo_transp[length(tipo_transp)])
      punto_rbind <- rbind(punto_rbind,df_punto_rbind)
  }
  return(punto_rbind)
}

z<-seleccion_puntos(distrito_origen= "Ciudad de Godoy Cruz")

