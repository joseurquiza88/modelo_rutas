#######################################################################
# ------------             BUSQUEDA DE LA GRILLA HORARIA     ------------- 

#######################################################################
# ------------             BUSQUEDA DE LA GRILLA HORARIA     ------------- 

#Esta funcion permite que al ingresarle 2 fechas distintas
# una de inicio y otra final obtenemos la grilla de interes 
# En el caso que las horas de las fechas sean distintas por ejemplo:
# hora_inicio <- "2023-01-19 06:50:00 -03"i
# hora_fin <- "2023-01-19 09:50:00 -03"
# La funcion busca las grillas disponibles para ese periodo (06,07,08,09)
# Y se genera una media pixel a pixel.

#La salida es un data.frame listo para guardar temporalmente en .shp

busqueda_grilla <- function(hora_inicio,hora_fin=NULL,directorio_grillas,formato_hora){
  #  --- Funcion que busca la grilla (.shp) correspondiente a la hora de interes ingresada
  # La grilla esta ubicada en una carpeta detaeminada
  grillas_horaria <- function(hora, formato_hora = formato_hora,directorio_grillas){
    hora_ingresada <- as.POSIXct(strptime(hora, format = formato_hora))
    hora_exposicion<- hour(hora_ingresada)
    setwd(directorio_grillas)
    lista_archivos <- dir(directorio_grillas,pattern = ".shp")
    hora_buscada <- which(as.numeric(substr(lista_archivos,8,9)) == hora_exposicion)
    archivo <- lista_archivos[hora_buscada] 
    #print(archivo )
    return(archivo)
  }
  grilla_trayecto_rbind <- data.frame()
  solo_hora_inicio <- hour(as.POSIXct(strptime(hora_inicio, format = formato_hora)))
  solo_hora_fin <- hour(as.POSIXct(strptime(hora_fin, format = formato_hora)))
  
  #  --- Hay veces que no ingresamos una hora-fin por ejemplo ?¡
  if (is.null(hora_fin)){
    df_grilla_inicio <- st_read(grillas_horaria(hora_inicio, formato_hora = formato_hora,directorio_grillas),quiet = TRUE)
  }
  #  --- Cuando es una sola grilla
  else if (solo_hora_inicio == solo_hora_fin ){
    grilla_trayecto <- st_read(grillas_horaria(hora_inicio, formato_hora = formato_hora,directorio_grillas),quiet = TRUE)
  }else{
    #  --- Cuando son varias grillas hacemos una media por pixel
    for(j in solo_hora_inicio:solo_hora_fin){
      
      if (j < 10){
        j_hora <- paste("0",j,sep = "")
      }else{
        j_hora <- j
      }
      
      dia <- paste(substr(hora_inicio,1,10),paste(j_hora,":00:00",sep = ""), "-03",sep = " ")
      #print (paste("GRILLA", j_hora,sep = " "))#
      grilla_trayecto <- st_read(grillas_horaria(dia, formato_hora = "%Y-%m-%d %H:%M:%S",directorio_grillas),quiet = TRUE)
      grilla_trayecto$hora <- dia
      grilla_trayecto_rbind <- rbind(grilla_trayecto_rbind,grilla_trayecto)
    }
    ## ------------ Agrupamos por el ID de la grilla y hacemos la media de cada pixel
    grilla_trayecto_rbind %>%
      group_by(GRI1_ID) %>%  
      group_split() -> data_grilla
    
    df_grilla <- data.frame()
    
    for (p in 1:length(data_grilla)){
      # if(p %%1000==0){
      #   print("Procesando grilla...")
      # }
      
      GRI1_ID <- data_grilla[[p]][["GRI1_ID"]][1]
      POBLXGRI <- mean(data_grilla[[p]][["POBLXGRI"]],na.rm = T)
      DISTANCIA <- mean(data_grilla[[p]][["DISTANCIA"]],na.rm = T)
      EMI_PST <- mean(data_grilla[[p]][["EMI_PST"]],na.rm = T)
      EMI_NOX <- mean(data_grilla[[p]][["EMI_NOX"]],na.rm = T)
      X_COORD <- data_grilla[[p]][["X_COORD"]][1]
      Y_COORD <- data_grilla[[p]][["Y_COORD"]][1]
      ALTURA_M_ <- mean(data_grilla[[p]][["ALTURA_M_"]],na.rm = T)
      PMBICIS <- mean(data_grilla[[p]][["PMBICIS"]],na.rm = T)
      PMDIARIO <- mean(data_grilla[[p]][["PMDIARIO"]],na.rm = T)
      PMHORARIO <- mean(data_grilla[[p]][["PMHORARIO"]],na.rm = T)
      geometry <- data_grilla[[p]][["geometry"]][1]
      len <- length(data_grilla[[p]][["geometry"]])
      
      df <- data.frame(GRI1_ID,POBLXGRI,DISTANCIA,EMI_PST,EMI_NOX ,
                       X_COORD,Y_COORD,ALTURA_M_,PMBICIS,PMDIARIO,
                       PMHORARIO, geometry,len)
      
      names(df) <- c("GRI1_ID","POBLXGRI","DISTANCIA","EMI_PST","EMI_NOX" ,
                     "X_COORD","Y_COORD","ALTURA_M_","PMBICIS","PMDIARIO",
                     "PMHORARIO", "geometry","len")
      df_grilla <- rbind(df_grilla ,df)
      names(df_grilla) <- c("GRI1_ID","POBLXGRI","DISTANCIA","EMI_PST","EMI_NOX" ,
                            "X_COORD","Y_COORD","ALTURA_M_","PMBICIS","PMDIARIO",
                            "PMHORARIO", "geometry","len")
    }
    st_write(df_grilla,"./temp/temp_grilla.shp",delete_layer = TRUE,quiet = TRUE)
    grilla_trayecto<- st_read("./temp/temp_grilla.shp",quiet = TRUE)
  }
  if(is.null(hora_fin)){
    #print("Funcion horas OK 1")
    return(df_grilla_inicio)
    }else{
     # print("Funcion busqueda_grilla  OK")
      return(grilla_trayecto)
    }
}

