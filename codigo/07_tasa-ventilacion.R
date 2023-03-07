## ---- Tasas de ventilacion
# Las concentraciones obtenidas por CALPUFF hacen referencia al exterior
# Por lo que es necesario ponerle un coeficientes mayor o menor si la persona 
# se encuentra dentro o fuera de espacios cerrados
# Tasa segun genero
tasa_ventilacion_genero <- function(genero){
  if(genero == "F"){
    tasa <- 2
  }
  else if(genero == "M"){
    tasa <- 1
  }
  return(tasa)
  
}
# Tasa segun modo
tasa_ventilacion_modo <- function(modo){
  
  if(modo == "Auto"){
    tasa <- 2
  }
  else if(modo == "Moto"){
    tasa <- 1
  }
  else if(modo == "Pie"){
    tasa <- 1
  }
  else if(modo == "Bicicleta"){
    tasa <- 1
  }
  else if(modo == "Colectivo"){
    tasa <- 1
  }
  else{
    tasa <-1
  }
  
  return(tasa)
  
}
# Tasa segun edad
tasa_ventilacion_edad <- function(edad){
  if(edad >= 0 & edad < 12){
    tipo <- "niño"
    tasa <- 1
  }
  else if(edad >= 12 & edad < 18){
    tipo <- "adolescente"
    tasa <- 1
  }
  else if(edad >=18  & edad < 40){
    tipo <- "adulto"
    tasa <- 2
  }
  else if(edad >= 40 & edad < 60){
    tipo <- "adulto B"
    tasa <- 1
  }
  else {
    tipo <- "adulto mayor"
    tasa <- 1
  }
  return (tasa)
}


tasa_ventilacion <- function(genero,edad,modo){
  tasa_genero <-  tasa_ventilacion_genero (genero)
  tasa_edad <-   tasa_ventilacion_edad(edad)
  tasa_modo <- tasa_ventilacion_modo(modo)
  df = data.frame(genero = tasa_genero,
             edad = tasa_edad,
             modo =tasa_modo)
  return(df)
}
 
tasa_ventilacion(genero="F",edad=20,modo="Auto")
  