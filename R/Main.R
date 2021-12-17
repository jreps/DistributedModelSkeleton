# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of DistributedModelSkeleton
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
#' This function executes the DistributedModelSkeleton Study.
#' 
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmDatabaseName      Shareable name of the database 
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target population cohorts used in this
#'                             study.
#' @param tempEmulationSchema  Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param runAnalyses          Run the model development
#' @param sampleSize           The number of patients in the target cohort to sample (if NULL uses all patients)
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
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
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cdmDatabaseName = 'shareable name of the database'
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         tempEmulationSchema = NULL,
#'         outputFolder = "c:/temp/study_results", 
#'         createCohorts = T,
#'         runAnalyses = T,
#'         sampleSize = 10000,
#'         packageResults = F,
#'         minCellCount = 5,
#'         verbosity = "INFO",
#'         cdmVersion = 5)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName = 'friendly database name',
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    tempEmulationSchema = cohortDatabaseSchema,
                    outputFolder,
                    createCohorts = F,
                    runAnalyses = F,
                    sampleSize = NULL,
                    packageResults = F,
                    minCellCount= 5,
                    verbosity = "INFO",
                    cdmVersion = 5
                    ) {
  
  if (!file.exists(outputFolder)){
    dir.create(outputFolder, recursive = TRUE)
  }
  
  if(createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      oracleTempSchema = oracleTempSchema,
      outputFolder = outputFolder
    )
  } 
  
  if(runAnalyses){
    ParallelLogger::logInfo("Running predictions")

    analysisListFile <- system.file(
      "settings",
      "analysisList.json",
      package = "DistributedModelSkeleton"
    )
    
    analysisList <- ParallelLogger::loadSettingsFromJson(analysisListFile)
    
    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails, 
      cdmDatabaseSchema = cdmDatabaseSchema, 
      cdmDatabaseName = cdmDatabaseName, 
      tempEmulationSchema = tempEmulationSchema, 
      cohortDatabaseSchema = cohortDatabaseSchema, 
      cohortTable = cohortTable, 
      outcomeDatabaseSchema = cohortDatabaseSchema, 
      outcomeTable = cohortTable, 
      cohortId = analysisList$targetId, 
      outcomeIds = analysisList$outcomeId, 
      cdmVersion = cdmVersion
      )
    
    covariateSettings <- getCovariateSettings(analysisList)
    restrictPlpDataSettings<- getRestrictPlpDataSettings(
      analysisList, 
      sampleSize
      ) 
    
    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails, 
      covariateSettings = covariateSettings, 
      restrictPlpDataSettings = restrictPlpDataSettings
      )
    
    populationSettings <- getPopulationSettings(analysisList) #make this
    
    labels <- PatientLevelPrediction::createStudyPopulation(
      plpData = plpData, 
      outcomeId = analysisList$outcomeId, 
      populationSettings = populationSettings
    )
    
    # convert to matrix
    
    dataObject <- PatientLevelPrediction::toSparseM(
      plpData = plpData, 
      cohort = labels
    )
    
    #sparse matrix:
    ##dataObject$dataMatrix
    
    #labels:
    ##dataObject$labels
    
    # covariateRef/covariateMap
    

  }
  
  if(packageResults) {
    ensure_installed("OhdsiSharing")
    ParallelLogger::logInfo("Packaging results")
    packageResults(dataObject,
                   outputFolder = outputFolder,
                   minCellCount = minCellCount
                   )
  }
  
  invisible(NULL)
}




