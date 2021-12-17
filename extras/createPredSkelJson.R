createStudyPackage <- function(
  packageName = 'exampleStudy',
  organizationName = 'add organization',
  targetId = 1,
  outcomeId = 2,
  dataStartDate = "",
  dataEndDate = "",
  populationSettings = list(),
  covariateSettings = list(),
  analysisJson = list(),
  baseUrl = 'https://...',
  outputFolder = getwd(),
  skeletonVersion = "v0.0.1"
){
  
  # check inputs
  
  jsonList <- createStudyJson(
    packageName = packageName,
    organizationName = organizationName,
    targetId = targetId,
    outcomeId = outcomeId,
    populationSettings = populationSettings,
    covariateSettings = covariateSettings,
    baseUrl = baseUrl
  )
  
    jsonList$skeletonVersion <- skeletonVersion
    json <- RJSONIO::toJSON(jsonList, digits = 23)
    Hydra::hydrate(json, outputFolder = outputFolder)

  return(invisible(NULL))
}


createStudyJson <- function(
  packageName = 'exampleStudy',
  organizationName = 'add organization',
  targetId = 1,
  outcomeId = 2,
  dataStartDate = "",
  dataEndDate = "",
  populationSettings = list(),
  covariateSettings = list(),
  analysisJson = list(),
  baseUrl = 'https://...'
){
  
  result <- list()
  
  result$skeletonType <-  "DistributedModelsSkeleton"
  result$packageName <- packageName
  
  result$organizationName <-  organizationName
  result$createdDate <- Sys.Date()
  
  result$targetId <- targetId
  result$outcomeId <- outcomeId
  
  result$dataStartDate <- dataStartDate
  result$dataEndDate <- dataEndDate
  
  result$populationSettings <- populationSettings
  
  result$covariateSettings <- covariateSettings
  
  result$analysisSpecification <- analysisJson 
  
  result$cohortDefinitions <- getCohorts(
    targetId, 
    outcomeId,
    covariateSettings,
    baseUrl
    )
  
  return(result)
}

# extracts the jsons for cohort each id
getCohorts <- function(targetId,
                       outcomeId, 
                       covariateSettings,
                       baseUrl){
  
  covariateIds <- unique(unlist(lapply(1:length(covariateSettings), function(i) {lapply(covariateSettings[[i]], function(x) x$settings$cohortId)})))
  cohortIds <- c(targetId, outcomeId, covariateIds)
  
  cohorts <- list()
  length(cohorts) <- length(cohortIds)
  for(i in 1:length(cohortIds)){
    cohorts[[i]] <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortIds[[i]], baseUrl = baseUrl)
  }
  return(cohorts)
}


#===================
# covariateSettings
#===================
createCohortCovariateSetting <- function(atlasId = 1,
                                         covariateName = '',
                                         startDay=-30,
                                         endDay=0,
                                         count=F,
                                         ageInteraction = F,
                                         lnAgeInteraction= F,
                                         analysisId = 456,
                                         points,
                                         offset = 0,
                                         power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createCohortCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     covariateId = atlasId*1000+analysisId,
                                     cohortId = atlasId,
                                     startDay = startDay,
                                     endDay = endDay,
                                     count = count,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     analysisId = analysisId))
  }else{
    settings <- list(fnct = 'createCohortCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     covariateId = atlasId*1000+analysisId,
                                     cohortId = atlasId,
                                     startDay = startDay,
                                     endDay = endDay,
                                     count = count,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = atlasId*1000+analysisId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}


createGenderCovariateSetting <- function(male = T,
                                         points,
                                         offset,
                                         power){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createCovariateSettings',
                     settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                           includedCovariateIds = ifelse(male, 8507, 8532)*1000+1))
  }else{
    settings <- list(fnct = 'createCovariateSettings',
                     settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                           includedCovariateIds = ifelse(male, 8507, 8532)*1000+1),
                     coeffs  = list(covariateId = ifelse(male, 8507, 8532)*1000+1,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}

createAgeCovariateSetting <- function(covariateName = 'Age at index',
                                      ageMap = function(x){return(x)},
                                      covariateId = 1458,
                                      analysisId = 458,
                                      points,
                                      offset = 0,
                                      power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createAgeCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     ageMap = ageMap,
                                     covariateId = covariateId,
                                     analysisId = analysisId))
  }else{
    settings <- list(fnct = 'createAgeCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     ageMap = ageMap,
                                     covariateId = covariateId,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = covariateId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}

createMeasurementCovariateSetting <- function(covariateName,
                                              conceptSet,
                                              startDay=-30,
                                              endDay=0,
                                              scaleMap = NULL,
                                              aggregateMethod = 'recent',
                                              imputationValue = 0,
                                              ageInteraction = F,
                                              lnAgeInteraction = F,
                                              lnValue = F,
                                              covariateId = 1466,
                                              analysisId = 466,
                                              points,
                                              offset = 0,
                                              power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createMeasurementCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     conceptSet = conceptSet,
                                     startDay=startDay,
                                     endDay=endDay,
                                     scaleMap = scaleMap,
                                     aggregateMethod = aggregateMethod,
                                     imputationValue = imputationValue,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     lnValue = lnValue,
                                     covariateId = covariateId,
                                     analysisId = analysisId))
  } else{
    settings <- list(fnct = 'createMeasurementCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     conceptSet = conceptSet,
                                     startDay=startDay,
                                     endDay=endDay,
                                     scaleMap = scaleMap,
                                     aggregateMethod = aggregateMethod,
                                     imputationValue = imputationValue,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     lnValue = lnValue,
                                     covariateId = covariateId,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = covariateId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
 
  
  return(settings)
}


# finish this
createStandardCovariateSetting <- function(
  
  
){
  settings <- list(fnct = 'FeatureExtraction::createCovariateSettings',
                   settings = settings)
  return(settings)
}

