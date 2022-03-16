Instructions To Run Study
===================
- Execute the study by running the code in (extras/CodeToRun.R) :
```r
library(SkeletonDistributedModel)
# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "C:/SkeletonDistributedModelResults"


# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'

# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'

tempEmulationSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'SkeletonPredictionStudyCohort'

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = cdmDatabaseSchema, 
  cdmDatabaseName = cdmDatabaseSchema,
  tempEmulationSchema = tempEmulationSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  cdmVersion = 5
)


# replace NULL with number to sample if needed
sampleSize <- NULL
#=======================

execute(
  databaseDetails = databaseDetails,
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
        )
```

The 'createCohorts' option will create the target and outcome cohorts into cohortDatabaseSchema.cohortTable if set to T.  

...
