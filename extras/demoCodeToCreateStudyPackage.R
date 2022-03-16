# demo code to create a study package:

## Make sure you have the latest Hydra
##devtools::install_github('ohdsi/Hydra')

outputFolder <- ''
baseUrl <- ''

json <- createStudyJson(
  packageName = 'exampleStudy',
  skeletonVersion = "v0.0.1",
  organizationName = 'testOrganization',
  targetId = 1,
  outcomeId = 2,
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
  populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
    firstExposureOnly = T, 
    requireTimeAtRisk = F, 
    riskWindowStart = 0, 
    riskWindowEnd = 30
  ),
  covariateSettings = list(
    PatientLevelPrediction::createCohortCovariateSettings(
    cohortName = 'Diabetes', 
    settingId = 1, 
    cohortId = , 
    startDay = -365*5, 
    endDay = -1,
    analysisId = 566
      ),
    PatientLevelPrediction::createCohortCovariateSettings(
      cohortName = 'COPD', 
      settingId = 1, 
      cohortId = , 
      startDay = -365*5, 
      endDay = -1,
      analysisId = 566
    ),
    ),
  control = list(
    heterogeneity = FALSE,
    model = 'ODAL',
    family = 'binomial',
    optim_maxit = 100
  ),
  baseUrl = baseUrl
)

# Hydra the package:
Hydra::hydrate(
  specifications = json, 
  outputFolder = outputFolder
  )

