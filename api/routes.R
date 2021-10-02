#* @apiTitle iRadiance API
#* @apiDescription Getting processed data from NASA POWER Portal. Data is processed using R.

# Libs
# library(markovifyR)
library(tidyverse)
library(jsonlite)
library(readr)
library(imputeTS)
library(rstudioapi) 

paramUrl = "https://power.larc.nasa.gov/api/temporal/daily/point?parameters=CLOUD_AMT_DAY,PRECTOTCORR,TS,ALLSKY_SFC_SW_DWN&community=RE&longitude=113.9970&latitude=4.3720&start=20210331&end=20210331&format=JSON"
print("geting params from url")
params = fromJSON(paramUrl)[["parameters"]]
print("params gotten")

#============retrieve and clean radiance data===========================
toJsonString = function(df){
  return(toJSON(df, pretty=TRUE))
}

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

getDailyDataDf = function(longitude = 113.9970, latitude = 4.3720, startTime = "20010101", endTime = "20210920"){
  #read daily csv for radiance related data
  url = gsub(" ", "", 
             paste("https://power.larc.nasa.gov/api/temporal/daily/point?parameters=CLOUD_AMT_DAY,ALLSKY_SFC_SW_DWN,TS,PRECTOTCORR&community=RE",
                "&longitude=", longitude,
                "&latitude=", latitude,
                "&start=", startTime,
                "&end=", endTime,
                "&format=CSV"))
  print("Getting daily data from api from NASA POWER Portal...")
  print(url)
  radiance.df<- read_csv(url, 
                          col_types = cols(YEAR = col_integer(), 
                          MO = col_integer(), DY = col_integer()), 
                          skip = 12)
  print("Received aily data.")
  print("Removing invalid value...")
  #to remove any -999 value, which means not applicable data
  for (i in 1:ncol(radiance.df)) {
    radiance.df[,i][radiance.df[,i] < 0] = NA
  }
  
  print("Imputing missing solar irradiance using Kalman Filter")
  x = radiance.df[, -which(names(radiance.df) == "CLOUD_AMT_DAY")]
  imp = na_kalman(x) #impute the missing solar irradiance using Kalman filter
  
  radiance.df$ALLSKY_SFC_SW_DWN = imp$ALLSKY_SFC_SW_DWN #give the solar irradiance column back to the dataset
  print("DONE! Returning daily data...")
  return(radiance.df)

}

getMonthlyDataDf = function(longitude = 113.9970, latitude = 4.3720, startTime = "20010101", endTime = "20210920"){
  # #read daily csv for radiance related data
  # longitude = 113.9970
  # latitude = 4.3720
  # startTime = "20210101"
  # endTime = "20210920"
  
  startYear = as.integer(substr(startTime, 1, 4))
  endYear = as.integer(substr(endTime, 1, 4))
  maxApiYear = 2020
  radiance.df = data.frame(PARAMETER = character(), YEAR = integer(), JAN = numeric(),FEB = numeric(),MAR = numeric(),
                           APR = numeric(),MAY = numeric(),JUN = numeric(),JUL = numeric(),AUG = numeric(),SEP = numeric(),OCT = numeric(),NOV = numeric(),DEC = numeric(),ANN = numeric())

  if(startYear < maxApiYear){
    url = paste("https://power.larc.nasa.gov/api/temporal/monthly/point?parameters=ALLSKY_SFC_SW_DWN,CLOUD_AMT_DAY,TS,PRECTOTCORR&community=RE",
               "&longitude=", longitude,
               "&latitude=", latitude,
               "&start=", ifelse(startYear>maxApiYear, maxApiYear,startYear),
               "&end=", ifelse(endYear>maxApiYear, maxApiYear,endYear),
               "&format=CSV",
               sep="")
    
    print("Getting monthly data from api from NASA POWER Portal...")
    print(url)
    
    radiance.df<- read_csv(url, 
                           col_types = cols(YEAR = col_integer()), 
                            skip = 12)
    print("Monthly data received.")
  }
  
  
  if(startYear > maxApiYear || endYear > maxApiYear){
    print("Need to get daily data 2021 for calculation")
    defaultStartYear = 2021
    tempDailyDf = getDailyDataDf(longitude = longitude, latitude = latitude, startTime = "20210101", endTime = endTime)
    print("Calculating monthly data using daily data...")
    for(i in defaultStartYear:endYear){
      year = i
      tempCloudDf = data.frame(PARAMETER = "CLOUD_AMT_DAY", YEAR = year, JAN = NA,FEB = NA,MAR = NA,
                               APR = NA,MAY = NA,JUN = NA,JUL = NA,AUG = NA,SEP = NA,OCT = NA,NOV = NA,DEC = NA,ANN = NA)
      tempSolarDf = data.frame(PARAMETER = "ALLSKY_SFC_SW_DWN",YEAR = year, JAN = NA,FEB = NA,MAR = NA,
                               APR = NA,MAY = NA,JUN = NA,JUL = NA,AUG = NA,SEP = NA,OCT = NA,NOV = NA,DEC = NA,ANN = NA)
      tempTemperatureDf = data.frame(PARAMETER = "TS",YEAR = year, JAN = NA,FEB = NA,MAR = NA,
                                     APR = NA,MAY = NA,JUN = NA,JUL = NA,AUG = NA,SEP = NA,OCT = NA,NOV = NA,DEC = NA,ANN = NA)
      tempPrecDf = data.frame(PARAMETER = "PRECTOTCORR",YEAR = year, JAN = NA,FEB = NA,MAR = NA,
                              APR = NA,MAY = NA,JUN = NA,JUL = NA,AUG = NA,SEP = NA,OCT = NA,NOV = NA,DEC = NA,ANN = NA)
      
      for(j in 1:12){
        mo = j
        mo_name = switch (as.character(mo),
                          '1' = "JAN",
                          '2' = "FEB",
                          '3' = "MAR",
                          '4' = "APR",
                          '5' = "MAY",
                          '6' = "JUN",
                          '7' = "JUL",
                          '8' = "AUG",
                          '9' = "SEP",
                          '10' = "OCT",
                          '11' = "NOV",
                          '12' = "DEC"
        )
        index = as.logical(c(tempDailyDf$YEAR == year) * c(tempDailyDf$MO == mo))
        
        testing = tempDailyDf[index,]
        
        tempCloudMean = mean(testing$CLOUD_AMT_DAY)
        tempSolarMean = mean(testing$ALLSKY_SFC_SW_DWN)
        tempTemperatureMean = mean(testing$TS)
        tempPrecMean = mean(testing$PRECTOTCORR)
        

        tempCloudDf[,colnames(tempCloudDf) == mo_name] = tempCloudMean
        tempSolarDf[,colnames(tempSolarDf) == mo_name] = tempSolarMean
        tempTemperatureDf[,colnames(tempTemperatureDf) == mo_name] = tempTemperatureMean
        tempPrecDf[,colnames(tempPrecDf) == mo_name] = tempPrecMean
      }
      radiance.df = rbind(radiance.df,tempCloudDf,tempSolarDf,tempTemperatureDf, tempPrecDf)
    }
    
    print("Monthly data calculation completed.")
    
  }
  print("Removing NAN value.")
  #remove NAN
  radiance.df[is.nan(radiance.df)] = NA
  
  # #sort it by param
  # radiance.df = radiance.df[order(radiance.df$PARAMETER),]
  
   print("DONE! Returning monthly data...")
  return(radiance.df)
}

getHourlyMeanDataDf = function(longitude = 113.9970, latitude = 4.3720, startTime = "20010101", endTime = "20210920"){
  # not implemented
}

#to change parameter from symbol to name
changeDailyParamName = function(df){
  for(i in 1:length(params)){
    paramName = names(params)[i]
    index = which(colnames(df) == paramName)
    colnames(df)[index] = params[[i]][["longname"]]
  }
  index = which(colnames(df) == "MO")
  colnames(df)[index] = "MONTH"
  index = which(colnames(df) == "DY")
  colnames(df)[index] = "DAY"
  
  return(df)
}

#to change parameter from symbol to name
changeMonthlyParamName = function(df){
  for(i in 1:length(params)){
    paramName = names(params)[i]
    index = which(df$PARAMETER == paramName)
    df$PARAMETER[index] = params[[i]][["longname"]]
  }
  return(df)
}

#to convert data frame to List
dfToList = function(df){
  return(split(df, seq(nrow(df))))
}

#' Default GET route. To verify connection with server.
#' @get /
function() {
  list(status = "OK")
}



#* Get processed daily data for Solar Irradiance, Cloud Amount, Precipitation, Temperature
#* @param longitude the longitude
#* @param latitude the latitude
#* @param startTime the start time
#* @param endTime the end time
#* @get /daily
#* @serializer json
function(longitude = 113.9970, latitude = 4.3720, startTime = "20010101", endTime = "20210920") {
  df = getDailyDataDf(longitude,latitude,startTime,endTime)
  # return(changeDailyParamName(df))
  return(df) #by default, data frame will be returned in JSON, by jsonlite
}

#* Get processed monthly data for Solar Irradiance, Cloud Amount, Precipitation, Temperature
#* @param longitude the longitude
#* @param latitude the latitude
#* @param startTime the start time
#* @param endTime the end time
#* @get /monthly
#* @serializer json
function(longitude = 113.9970, latitude = 4.3720, startTime = "20010101", endTime = "20210920") {
  df = getMonthlyDataDf(longitude,latitude,startTime,endTime)
  # return(changeMonthlyParamName(df))
  return(df) #by default, data frame will be returned in JSON, by jsonlite
}


