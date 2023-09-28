cambio_undades_pp <- function(img) {
  
  Precip <-  img$expression(
    'P * 1000', list(
      P= img$select('total_precipitation_sum')
    )
  )$rename(paste0('Precipitacion', "_"))
  
  
  img$addBands(Precip)$select(paste0('Precipitacion', "_"))
  
  
}



cambio_undades_Temperature <- function(img) {
  
  Temp = img$expression(
    'T - 273.15', list(
      T= img$select('temperature_2m')
    )
  )$rename(paste0('Temperatura', "_"))
  
  img$addBands(Temp)$select(paste0('Temperatura', "_"))
  
}


cambio_undiades_evaporation <- function(img){
  
  Tot_evaporation = img$expression(
    'te * 1000', list(
      te = img$select('total_evaporation_sum')
    )
  )$rename(paste0('total evaporation', "_"))
  
  
  img$addBands(Tot_evaporation)$select(paste0('total evaporation', "_"))
  
  
}


cambio_unidades_dewpoint <- function(img){
  
  Punto_rocio = img$expression(
    'Td - 273.15', list(
      Td= img$select('dewpoint_temperature_2m')
    )
  )$rename(paste0('Punto rocio', "_"))
  
  
  
  img$addBands(Punto_rocio)$ select(paste0('Punto rocio', "_"))
}


cambio_unidades_wind <-  function(img){
  
  Viento = img$expression(
    'sqrt((u*3.6)**2 + (v*3.6)**2)', list(
      u = img$select('u_component_of_wind_10m'),
      v = img$select('v_component_of_wind_10m')
    )
  )$rename(paste0('Viento', "_"))
  
  direccion_viento <- img$expression(
    'mod(180 + (180/pi) * atan2(v,u), 360)', list(
      u = img$select('u_component_of_wind_10m'),
      v = img$select('v_component_of_wind_10m'),
      pi = pi
    )
  )$rename(paste0('DirViento', "_"))
  
  
  img$addBands(Viento)$
      addBands(direccion_viento)$
    select(paste0('Viento', "_"),
           paste0('DirViento', "_"))
  
}


cambio_unidades_hr <- function(img) {
  
  Temp = img$expression(
    'T - 273.15', list(
      T= img$select('temperature_2m')
    )
  )$rename(paste0('Temperatura', "_"))
  
  Punto_rocio = img$expression(
    'Td - 273.15', list(
      Td= img$select('dewpoint_temperature_2m')
    )
  )$rename(paste0('Punto rocio', "_"))
  
  
  Hr = img$expression(
    '100 * (exp(17.625*dp/(243.04 + dp ))/exp(17.625 *t/(243.04 + t )))', list(
      dp = Punto_rocio,
      t = Temp
    )
  )$rename(paste0('Humedad relativa', "_"))
  
  
  img$addBands(Hr)$select(paste0('Humedad relativa', "_"))
  
}





