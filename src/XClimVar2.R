library(sf)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
library(purrr)
library(rgee)

#####=======>>>>> Library XCV <<<<<<=======######

calculate_year_ranges <- function(time_col, p_length, step){
  # Takes a date , the length of the period and the steps to complete it
  # and sequence the period. Returns the period
  
  start <- as.Date(time_col)
  seq(start, start %m+% years(p_length), paste(step,'year'))
}

calculate_monthly_ranges <-
  function(time_col,
           p_length = 3,
           step = 1) {
    # Takes a date , the length of the period and the steps to complete it
    # and sequence the period. Returns the period
    
    inicio <- as.Date(time_col)
    seq(inicio, inicio %m+% months(p_length), paste(step, 'month'))
  }

calculate_Daily_ranges <- function (time_col ,
                                    p_length = 3,
                                    step = 1) {
  # Takes a date , the length of the period and the steps to complete it
  # and sequence the period. Returns the period
  
  start <- as.Date(time_col)
  seq(start, start %m+% days(p_length), paste(step, "day"))
}
# ====>> Time Calc Function <<====
time_calc <-
  function(dsf_time,
           Yearly = FALSE,
           Monthly = FALSE,
           Daily =FALSE,
           p_length = 6,
           step = 3) {
    ### compares if the period is year, month or day and then generates a sequence 
    ### of dates and saves it in a list 
    
    if ((Yearly == TRUE &
         Monthly == FALSE &
         Daily == FALSE) ||
        (Yearly == FALSE &
         Monthly == TRUE &
         Daily == FALSE) ||
        (Yearly == FALSE & Monthly == FALSE & Daily == TRUE))
    {
      if (Yearly == TRUE) {
        listafechas <-
          lapply(sort(unique(dsf_time)), function(x) {
            calculate_year_ranges(x, p_length, step)
          })
        return (listafechas)
      }
      if (Monthly == TRUE) {
        listafechas <-
          lapply(sort(unique(dsf_time)), function(x) {
            calculate_monthly_ranges(x, p_length, step)
          })
        return (listafechas)
      }
      if (Daily == TRUE) {
        listafechas <-
          lapply(sort(unique(dsf_time)), function(x) {
            calculate_Daily_ranges(x, p_length, step)
          })
        return (listafechas)
      }
    } else {
      stop("Select only one type of date to procces with TRUE")
    }
    
  }


#====>>Extract Period Function Daily <<=======================================================================


extract_periodoD <- function (dsf ,dsf_time , pol , listaf=listafechas, climaticvar,  m=1){
  #function that iterates through a vector that contains dates, every two pairs of dates (startDate, endDate)
  #filter the data for the first date in the vector and download the data for that period and then
  #puts it in a dataframe, after that takes the second pair and makes  the same but it 
  #makes a cbind with the data frame with the previews data and so on.
  #It returns a data frame 
  
  datos_periodicos <- data.frame()
  counter <- 0
  orcol <- dim(dsf)[2]
  for(x in seq(1,length(pluck(listaf,m)))){ # Loop for that iterates through a vector 
    counter <-counter+1
    
    startDate <- format(as.Date(pluck(listaf,m,x)),'%Y-%m-%d')
    
    if(!is.null(pluck(listaf,m,x+1))){
      endDate <- format(as.Date(pluck(listaf,m,x+1)),'%Y-%m-%d')
      
    } else{
      break
    }
    
    ## Data extraction
    ## "ECMWF/ERA5_LAND/DAILY_RAW" gives the correct total pp and evaporation of the day https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_RAW
    
    if (startDate >= endDate)
      stop("Error")
    
    cat("Starting date:", startDate, " - ",endDate, "\n\n")
    
    ### ========>> Precipitation extaction   <<=================================
    
    
        ##===>> Precipitation Sum <<====
    if ('pp_sum' %in% climaticvar){   
    print('pp_sum')
    var_to_downl <- c('total_precipitation_sum')
    eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
      filter(ee$Filter$date(startDate,endDate))$                   
      select(var_to_downl)$                          ## limits the data to the poligon 
      filterBounds(pol)$
      map(cambio_undades_pp)$            ## transforms the mesurements 
      reduce(ee$Reducer$sum())  
    
    if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
      filtered_data <- dsf[dsf_time == startDate,] 
    }
    
    ee_pp <-                  
      ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
    
    colnames(ee_pp) <-        #removes the x000 from the colnames
      gsub('X[0-9]{8}_',
           '',
           colnames(ee_pp),
           fixed = FALSE,
      )
    colnames(ee_pp) <-      #removes the _month and puts a number instead
      gsub('__sum',
           paste0('_','S',counter),
           colnames(ee_pp)
      )
    
    if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
      datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
    } else{                                         # the rest of the data makes a cbind with the firs dataframe
      ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
      ee_pp <- st_drop_geometry(ee_pp)
      datos_periodicos <- cbind(datos_periodicos, ee_pp)
    }
    
    }
    
        ## =====>> Precipitation Mean <<=====
    
    if ('pp_mean' %in% climaticvar){   
      print('pp_mean')
      var_to_downl <- c('total_precipitation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_pp)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())  
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      }
      
    
    ### ==========>>>Temperature extraction <<<=================================
    
      ### =========>>> Temperature sum <<<=============
    
    if ('t_sum' %in% climaticvar){ 
      print('t_sum')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
      ### ===========>>> Temperature mean <<<===========
    
    if ('t_mean' %in% climaticvar){ 
      print('t_mean')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    ### =========>>>> Total evaporation extraction <<<<=========================
    
    
    
      #### ======>>> Total evaporation sum <<<=========
    
    if ('te_sum' %in% climaticvar){ 
      print('te_sum')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
     
    
    
      ### ==========>>>  Total evaporation mean <<<================
    
    if ('te_mean' %in% climaticvar){ 
      print('te_mean')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
    
    ###===========>>>> Dewpoint extraction <<<==============
    
    
    if ('dp' %in% climaticvar){
      print('dp')
      var_to_downl <- c('dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_dewpoint)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Relative humidity extraction <<<==============
    
    
    if ('hr' %in% climaticvar){
      print('hr')
      var_to_downl <- c('temperature_2m',
                        'dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_hr)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Wind extraction <<<==============
    
    
    if ('wind' %in% climaticvar){
      print('wind')
      var_to_downl <- c('u_component_of_wind_10m',
                        'v_component_of_wind_10m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_wind)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
  }
  dim <- dim(datos_periodicos)
  print(paste0("Dimentions" ,'==> ',paste0(dim[1],'-',dim[2])))
  return(datos_periodicos)
}

#====>> Extract data function Daily <<=====

extract_dataD <- function(dsf ,dsf_time , pol, climaticvar,listaf=listafechas){
  ## Extracts the data from a list of vectors that contains a sequence of dates 
  ## uses the function "extract_period" to extract the period of the vector and 
  ## then unifies them in a dataframe with the rest  of different sequeces
  datos_finales = data.frame()
  count = 0
  for (l in seq(1,length(listaf))){ 
    count=count+1
    print(count)
    datos_per <- extract_periodoD(dsf ,dsf_time , pol , listaf ,climaticvar, m=l)
    if (count == 1){
      datos_finales = datos_per 
    }else{
      colnames(datos_per) <- colnames(datos_finales) ## col names are reasigned because the first rbind changes the names
      datos_finales  = rbind(datos_finales,datos_per)}
    
    
  }
  return(datos_finales)
}


#====>>Extract Period Function Monthly<<=====


extract_periodoM <- function (dsf ,dsf_time , pol, listaf=listafechas,climaticvar, m=1 ){
  #function that iterates through a vector that contains dates, every two pairs of dates (startDate, endDate)
  #filter the data for the first date in the vector and download the data for that period and then
  #puts it in a dataframe, after that takes the second pair and makes  the same but it 
  #makes a cbind with the data frame with the previews data and so on.
  #It returns a data frame 
  
  datos_periodicos <- data.frame()
  counter <- 0
  orcol <- dim(dsf)[2]
  for(x in seq(1,length(pluck(listaf,m)))){ # Loop for that iterates through a vector 
    counter <-counter+1
    
    startDate <- format(as.Date(pluck(listaf,m,x)),'%Y-%m-%d')
    
    if(!is.null(pluck(listaf,m,x+1))){
      endDate <- format(as.Date(pluck(listaf,m,x+1)),'%Y-%m-%d')
      
    } else{
      break
    }
    
    ## Data extraction
    ## "ECMWF/ERA5_LAND/DAILY_RAW" gives the correct total pp and evaporation of the day https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_RAW
    
    if (startDate >= endDate)
      stop("Error")
    
    cat("Starting date:", startDate, " - ",endDate, "\n\n")
    
    ### ========>> Precipitation extaction   <<=================================
    
    
    ##===>> Precipitation Sum <<====
    if ('pp_sum' %in% climaticvar){   
      print('pp_sum')
      var_to_downl <- c('total_precipitation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_pp)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())  
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ## =====>> Precipitation Mean <<=====
    
    if ('pp_mean' %in% climaticvar){   
      print('pp_mean')
      var_to_downl <- c('total_precipitation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_pp)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())  
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
    }
    
    
    ### ==========>>>Temperature extraction <<<=================================
    
    ### =========>>> Temperature sum <<<=============
    
    if ('t_sum' %in% climaticvar){ 
      print('t_sum')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ### ===========>>> Temperature mean <<<===========
    
    if ('t_mean' %in% climaticvar){ 
      print('t_mean')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    ### =========>>>> Total evaporation extraction <<<<=========================
    
    
    
    #### ======>>> Total evaporation sum <<<=========
    
    if ('te_sum' %in% climaticvar){ 
      print('te_sum')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
    ### ==========>>>  Total evaporation mean <<<================
    
    if ('te_mean' %in% climaticvar){ 
      print('te_mean')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
    
    ###===========>>>> Dewpoint extraction <<<==============
    
    
    if ('dp' %in% climaticvar){
      print('dp')
      var_to_downl <- c('dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_dewpoint)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Relative humidity extraction <<<==============
    
    
    if ('hr' %in% climaticvar){
      print('hr')
      var_to_downl <- c('temperature_2m',
                        'dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_hr)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Wind extraction <<<==============
    
    
    if ('wind' %in% climaticvar){
      print('wind')
      var_to_downl <- c('u_component_of_wind_10m',
                        'v_component_of_wind_10m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_wind)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
  }
  dim <- dim(datos_periodicos)
  print(paste0("Dimentions" ,'==> ',paste0(dim[1],'-',dim[2])))
  return(datos_periodicos)
}


#====>> Extract data function Monthly <<=====

extract_dataM <- function(dsf ,dsf_time , pol,climaticvar, listaf=listafechas){
  ## Extracts the data from a list of vectors that contains a sequence of dates 
  ## uses the function "extract_period" to extract the period of the vector and 
  ## then unifies them in a dataframe with the rest  of different sequeces
  datos_finales = data.frame()
  count = 0
  for (l in seq(1,length(listaf))){ 
    count=count+1
    print(count)
    datos_per <- extract_periodoM(dsf ,dsf_time , pol, listaf ,climaticvar, m=l)
    if (count == 1){
      datos_finales = datos_per 
    }else{
      colnames(datos_per) <- colnames(datos_finales) ## col names are reasigned because the first rbind changes the names
      datos_finales  = rbind(datos_finales,datos_per)}
    
    
  }
  return(datos_finales)
}



#====>>Extract Period Function Yearly<<=====


extract_periodoY <- function (dsf ,dsf_time , pol=poligono_ee, listaf=listafechas ,climaticvar, m=1){
  #function that iterates through a vector that contains dates, every two pairs of dates (startDate, endDate)
  #filter the data for the first date in the vector and download the data for that period and then
  #puts it in a dataframe, after that takes the second pair and makes  the same but it 
  #makes a cbind with the data frame with the previews data and so on.
  #It returns a data frame 
  
  datos_periodicos <- data.frame()
  counter <- 0
  orcol <- dim(dsf)[2]
  for(x in seq(1,length(pluck(listaf,m)))){ # Loop for that iterates through a vector 
    counter <-counter+1
    
    startDate <- format(as.Date(pluck(listaf,m,x)),'%Y-%m-%d')
    
    if(!is.null(pluck(listaf,m,x+1))){
      endDate <- format(as.Date(pluck(listaf,m,x+1)),'%Y-%m-%d')
      
    } else{
      break
    }
    
    ## Data extraction
    ## "ECMWF/ERA5_LAND/DAILY_RAW" gives the correct total pp and evaporation of the day https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_RAW
    
    if (startDate >= endDate)
      stop("Error")
    
    cat("Starting date:", startDate, " - ",endDate, "\n\n")
    
    ### ========>> Precipitation extaction   <<=================================
    
    
    ##===>> Precipitation Sum <<====
    if ('pp_sum' %in% climaticvar){   
      print('pp_sum')
      var_to_downl <- c('total_precipitation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_pp)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())  
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ## =====>> Precipitation Mean <<=====
    
    if ('pp_mean' %in% climaticvar){   
      print('pp_mean')
      var_to_downl <- c('total_precipitation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_pp)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())  
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      if(counter == 1 & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
    }
    
    
    ### ==========>>>Temperature extraction <<<=================================
    
    ### =========>>> Temperature sum <<<=============
    
    if ('t_sum' %in% climaticvar){ 
      print('t_sum')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ### ===========>>> Temperature mean <<<===========
    
    if ('t_mean' %in% climaticvar){ 
      print('t_mean')
      var_to_downl <- c('temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undades_Temperature)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    ### =========>>>> Total evaporation extraction <<<<=========================
    
    
    
    #### ======>>> Total evaporation sum <<<=========
    
    if ('te_sum' %in% climaticvar){ 
      print('te_sum')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$sum())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__sum',
             paste0('_','S',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
    ### ==========>>>  Total evaporation mean <<<================
    
    if ('te_mean' %in% climaticvar){ 
      print('te_mean')
      var_to_downl <- c('total_evaporation_sum')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_undiades_evaporation)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0  ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
    
    ###===========>>>> Dewpoint extraction <<<==============
    
    
    if ('dp' %in% climaticvar){
      print('dp')
      var_to_downl <- c('dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_dewpoint)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Relative humidity extraction <<<==============
    
    
    if ('hr' %in% climaticvar){
      print('hr')
      var_to_downl <- c('temperature_2m',
                        'dewpoint_temperature_2m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_hr)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    ###===========>>>> Wind extraction <<<==============
    
    
    if ('wind' %in% climaticvar){
      print('wind')
      var_to_downl <- c('u_component_of_wind_10m',
                        'v_component_of_wind_10m')
      eras_re <-   ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$   
        filter(ee$Filter$date(startDate,endDate))$                   
        select(var_to_downl)$                          ## limits the data to the poligon 
        filterBounds(pol)$
        map(cambio_unidades_wind)$            ## transforms the mesurements 
        reduce(ee$Reducer$mean())                           
      
      if (counter == 1  & dim(datos_periodicos)[1] == 0 ){ 
        filtered_data <- dsf[dsf_time == startDate,] 
      }
      
      
      ee_pp <-                  
        ee_extract(x = eras_re, y = filtered_data, sf = TRUE, scale = 1000) # extracts the data with the dataframe filtered
      
      colnames(ee_pp) <-        #removes the x000 from the colnames
        gsub('X[0-9]{8}_',
             '',
             colnames(ee_pp),
             fixed = FALSE,
        )
      colnames(ee_pp) <-      #removes the _month and puts a number instead
        gsub('__mean',
             paste0('_','M',counter),
             colnames(ee_pp)
        )
      
      
      if(counter == 1  & dim(datos_periodicos)[1] == 0 ){
        datos_periodicos <- rbind(datos_periodicos, ee_pp)   #the first data takes the place of the empty dataframe
      } else{                                         # the rest of the data makes a cbind with the firs dataframe
        ee_pp <- ee_pp[,orcol:(dim(ee_pp)[2])] # avoids multiplication of columns 
        ee_pp <- st_drop_geometry(ee_pp)
        datos_periodicos <- cbind(datos_periodicos, ee_pp)
      }
      
    }
    
    
    
  }
  dim <- dim(datos_periodicos)
  print(paste0("Dimentions" ,'==> ',paste0(dim[1],'-',dim[2])))
  return(datos_periodicos)
}


#====>> Extract data function Yearly <<=====

extract_dataY <- function(dsf ,dsf_time , pol,climaticvar , listaf=listafechas){
  ## Extracts the data from a list of vectors that contains a sequence of dates 
  ## uses the function "extract_period" to extract the period of the vector and 
  ## then unifies them in a dataframe with the rest  of different sequeces
  datos_finales = data.frame()
  count = 0
  for (l in seq(1,length(listaf))){ 
    count=count+1
    print(count)
    datos_per <- extract_periodoY(dsf ,dsf_time , pol, listaf, climaticvar , m=l)
    if (count == 1){
      datos_finales = datos_per 
    }else{
      colnames(datos_per) <- colnames(datos_finales) ## col names are reasigned because the first rbind changes the names
      datos_finales  = rbind(datos_finales,datos_per)}
    
    
  }
  return(datos_finales)
}




# ====>> Extract climate variables function <<====

XClimVar <- function(dsf,dsf_t,pol,climaticvar, Yearly = FALSE,
                     Monthly = FALSE,
                     Daily =FALSE,
                     p_length = 0,
                     step = 0
                     ){
  ## this function unifies the rest of the functions 
  ## First generates the list with the sequences of dates 
  ## Second unifies the periods 
  ## returns the original dataframe with the extra columns that contains the climatic variables
  if (p_length == 0 |step == 0){
      return(print('Select a period_length and a Step'))
  }else { 
    if (st_crs(dsf)$input == "EPSG:4326"){ #check if the coordinates system is correct!
      dates_list <- time_calc(dsf_time =dsf_t,Yearly ,Monthly, Daily ,p_length=p_length ,step=step  )
    
      if (Daily){ 
        print('daily')
        ready <- extract_dataD(dsf,dsf_t,pol, listaf=dates_list,climaticvar)
      } else if (Monthly) {
        print('Monthly')
        ready <- extract_dataM(dsf,dsf_t,pol,listaf=dates_list,climaticvar)
      } else if (Yearly){
        print('Yearly')
        ready <- extract_dataY(dsf,dsf_t,pol,listaf=dates_list,climaticvar)
      } else {
        print('Select the temporal unit')
        return()
      }
      print(paste0('TDimentions ==>',dim(ready)[1],'-',dim(ready)[2]))
      return(ready)
    } else {
        return(print('Set the correct coordinate system to: EPSG:4326'))
      }
  
 }
  
}

