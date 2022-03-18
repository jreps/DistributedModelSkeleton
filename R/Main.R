# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonDistributedModel
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute the Study
#'
#' @details
#' This function executes the SkeletonDistributedModel Study.
#' 
#' @param databaseDetails      The connection details and OMOP CDM details. Created using \code{PatientLevelPrediction::createDatabaseDetails}.
#' @param siteId               The name of your site (can be the university name, site with a number, etc.) tnis needs to be shared with the study administrator
#' @param uri                  The Universal Resource Identifier for the cloud
#' @param secret               The password to authenticate as siteId on uri
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param createData           Create the labelled data set (this is required before runAnalysis)
#' @param sampleSize           The number of patients in the target cohort to sample (if NULL uses all patients)
#' @param createControl        (for the lead site only) Run this code to create the study control
#' @param siteIds              (for the lead site only) vector with the names of all the sites contributing to the study. Required when createControl = TRUE
#' @param leadSiteNextStep     (for the lead site only) Run this when you are ready to move to the next step
#' @param runAnalysis          Runs the initialization, derive and estimate.  This needs to be run multiple times - the study administrator will inform you when you run.  This step 
#'                             involves downloading the latest control json (with model specifications), fitting the model locally and then uploading the model (coefficients and ?) to 
#'                             the cloud for the study administrator to combine.  Please note: no patient level data are transferred.
#' @param runSynthesize        Once the site estimates are returned, it is now possible to apply each model to the data to calculate
#'                             predictions.                          
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                              
#' @param cdmVersion           The version of the common data model                       
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(databaseDetails,
#'         siteId = 'site_1',
#'         uri = 'get from administrator',
#'         secret = 'hidden',
#'         outputFolder = "c:/temp/study_results", 
#'         createCohorts = T,
#'         createData = T,
#'         sampleSize = 10000,
#'         createControl = F,
#'         siteIds = c('site_1', 'site_2'),
#'         leadSiteNextStep = F,
#'         runAnalysis = F,
#'         runSynthesize = F,
#'         verbosity = "INFO",
#'         cdmVersion = 5)
#' }
#'
#' @export
execute <- function(databaseDetails,
                    siteId = 'site_name',
                    uri,
                    secret,
                    outputFolder,
                    createCohorts = F,
                    createData = F,
                    sampleSize = NULL,
                    createControl = F,
                    siteIds = '', # only needed if lead site
                    leadSiteNextStep = F,
                    runAnalysis = F,
                    runSynthesize = F,
                    verbosity = "INFO",
                    cdmVersion = 5
                    ) {
  
  if (!file.exists(outputFolder)){
    dir.create(outputFolder, recursive = TRUE)
  }
  
  # load the analysis
  analysisListFile <- system.file(
    "settings",
    "analysisList.json",
    package = "SkeletonDistributedModel"
  )
  
  if(file.exists(analysisListFile)){
    analysisList <- loadJson(analysisListFile)
    analysisList <- fromJsonFormat(analysisList)
    analysisList$studySettings$covariateSettings <- addCohortSettings(
      covariateSettings = analysisList$studySettings$covariateSettings,
      cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema , 
      cohortTable = databaseDetails$cohortTable
    )
  }else{
    stop('No analysisList available')
  }
  
  if(createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(
      databaseDetails = databaseDetails,
      outputFolder = outputFolder
    )
  } 
  
  if(createData){
    ParallelLogger::logInfo("Creating labelled dataset")

    databaseDetails$cohortId <- analysisList$studySettings$targetId
    databaseDetails$outcomeIds <- analysisList$studySettings$outcomeId
    
    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails, 
      covariateSettings = analysisList$studySettings$covariateSettings, 
      restrictPlpDataSettings = analysisList$studySettings$restrictPlpDataSettings
      )

    labels <- PatientLevelPrediction::createStudyPopulation(
      plpData = plpData, 
      outcomeId = analysisList$studySettings$outcomeId, 
      populationSettings = analysisList$studySettings$populationSettings
    )
    
    # convert to matrix
    
    dataObject <- PatientLevelPrediction::toSparseM(
      plpData = plpData, 
      cohort = labels
    )
    
    #sparse matrix: dataObject$dataMatrix
    #labels: dataObject$labels
    
    columnDetails <- merge(
      dataObject$covariateMap, 
      as.data.frame(dataObject$covariateRef), 
      by = 'covariateId'
    )
    cnames <- columnDetails$covariateName[order(columnDetails$columnId)]
    
    ipdata <- as.data.frame(dataObject$dataMatrix)
    colnames(ipdata) <- cnames
    ipdata$outcome <- dataObject$labels$outcomeCount
    
    # save the data:
    utils::write.csv(
      x = ipdata, 
      file = file.path(outputFolder, 'data'), 
      row.names = F
      )
  }
  
  # Step 1: lead site creates the control
  if(createControl){
    
    ParallelLogger::logInfo('Creating the control settings')
    
    #check the data exist to get the names
    if(dir.exists(file.path(outputFolder, 'data'))){
    data <- utils::read.csv(file.path(outputFolder, 'data'))
    } else{
      stop('Please generate data before creating control')
    }
    
    control <- list(
      project_name = analysisList$packageName,
      step = 'initialize',
      sites = siteIds,
      heterogeneity = analysisList$studySettings$control$heterogeneity,
      model = analysisList$studySettings$control$model,
      family = analysisList$studySettings$control$family,
      outcome = "outcome",
      variables = colnames(data)[colnames(data)!='outcome'],
      optim_maxit = analysisList$studySettings$control$optim_maxit,
      lead_site = siteId,
      upload_date = as.character(Sys.time()) 
    )
    
    # send the control to the cloud
    pda::pda(
      site_id = siteId, 
      control = control, 
      uri = uri, 
      secret = secret
      )
  }
  
  if(leadSiteNextStep){
    # if the lead site is ready to go to next step
    config <- pda::getCloudConfig(
      site_id = siteId,
      uri = uri, 
      secret = secret
      )
    pda::pdaSync(config)
  }
  
  if(runAnalysis){
    ipdata <- utils::read.csv(file.path(outputFolder, 'data'))
    
    config <- pda::getCloudConfig(
      site_id = siteId,
      uri = uri, 
      secret = secret
    )
    
    control <- tryCatch({
      pda::pdaGet(
      name = paste0('control'), 
      config = config
    )}, error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    
    if(!is.null(control)){
    
    ParallelLogger::logInfo('At step ', control$step)
      pda::pda(
        ipdata = ipdata, 
        site_id = siteId, 
        uri = uri, 
        secret = secret
      )
    } # control exists
  }
  
  if(runSynthesize){
    ipdata <- utils::read.csv(file.path(outputFolder, 'data'))
    
    config <- pda::getCloudConfig(
      site_id = siteId,
      uri = uri, 
      secret = secret
    )
    
    control <- pda::pdaGet(
      name = paste0('control'), 
      config = config
    )
    
    if(control$step == 'synthesize'){
      
      # apply each model to the data
      for(site in control$sites){
        fit.odal <- pda::pdaGet(
          name = paste0(site,'_estimate'), 
          config = config
        )
        
        # do something with this?
        cbind(b.odal=fit.odal$btilde,
              sd.odal=sqrt(diag(solve(fit.odal$Htilde)/nrow(ipdata))))
      }
    }
  }
  
  invisible(NULL)
}




