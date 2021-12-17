getCovariateSettings <- function(
  analysisList, 
  covariateDatabaseSchema, 
  covariateTable
  ){
  
  settingList <- analysisList$covariateSettings
  
  covariateList <- lapply(
    settingList, 
   function(x){do.call(
     'evaluateCovariateSetting', 
     list(fnct = x$fnct,
       settings = x$settings, 
       cohortDatabaseSchema = covariateDatabaseSchema,
       cohortTable = covariateTable
       )
     )}
   )
  
  return(covariateList)
}

evaluateCovariateSetting <- function(fnct, settings, cohortDatabaseSchema, cohortTable){
  
  if(fnct %in% c('createCohortCovariateSettings')){
    settings$cohortDatabaseSchema  <- covariateDatabaseSchema
    settings$cohortTable <- covariateTable
  }
  
  result <- do.call(
    eval(parse(text = fnct)), 
    settings
  )
  
  return(result)
}

getRestrictPlpDataSettings <- function(analysisList, sampleSize){
  
  result <- PatientLevelPrediction::createRestrictPlpDataSettings(
    studyStartDate = analysisList$dataStartDate, 
    studyEndDate = analysisList$dataEndDate, 
    sampleSize = sampleSize
    )
  
  return(result)
}

getPopulationSettings <- function(analysisList){
  result <- analysisList$populationSettings
  return(result)
}





packageResults <- function(
  dataObject,
  outputFolder = outputFolder,
  minCellCount = minCellCount
){
  
  # code to convert the data into the format for the study...
  
  
}