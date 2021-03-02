# Install RODBC package
#install.packages("RODBC")

# Load package
library(RODBC)

# Specify database path
MDBPATH <- "extdata/Grunddaten_ohne_Adressen.accdb"

## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
channel <- odbcDriverConnect(PATH)

# Table names
TABLES <- c("EURING_species_codes", "lutKeyValues", "tblSite")

## Function to extract data.frame of table
get_table <- function(tbl){
  
  ## Retrieve all variable names from table tbl
  tbl_vars <- sqlColumns(channel, tbl)["COLUMN_NAME"]
  
  ## Add brackets to each variable (ie. [variable]) to maintain ACCESS syntax
  tbl_vars$COLUMN_NAME <- paste0("[", tbl_vars$COLUMN_NAME, "]")
  
  ## Transform dataframe column into string separated by comma
  cols <- paste0(tbl_vars[1:nrow(tbl_vars), ], collapse=",")
  
  ## Extract table of interest as dataframe
  df <- sqlQuery(channel,
                 paste0("SELECT ", cols, " FROM [", tbl, "]", "", ";"),
                 stringsAsFactors=FALSE)
  return(df)
}

# Extract individual data.tables from accdb file
EURING_species_codes <- get_table(TABLES[1])
lutKeyValues <- get_table(TABLES[2])
tblSite <- get_table(TABLES[3])

## Close and remove channel
close(channel)
rm(channel)

# Now you can work with your data.frames

