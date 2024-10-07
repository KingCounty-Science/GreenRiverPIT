

## Connect to the PIT-tagging database and query data for the 2024 analysis ##


#### Load packages ####

if(!require(data.table)){install.packages('data.table'); library(data.table)} # Data manipulation package
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} # For the 'recode' function
if(!require(lubridate)){install.packages('lubridate'); library(lubridate)} # Easy date functions
if(!require(DBI)){install.packages('DBI')}; library(DBI) # For database connectivity
if(!require(dataRetrieval)){install.packages('dataRetrieval')}; library(dataRetrieval) # USGS data retrieval functions
if(!require(tidyr)){install.packages('tidyr')}; library(tidyr) # For data wrangling (spread(), gather())
if(!require(ggplot2)){install.packages('ggplot2')}; library(ggplot2) # For pretty plotting


#### Connect to the PIT-tag database and query 2024 data ####

DB_conn <- dbConnect(odbc::odbc(),
                     .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                 DBQ=P:/GregersenC/WRIA9/PIT_STUDIES/PIT_Database/Backend/PIT_tag_database_v1.0_be.accdb")

dbListTables(DB_conn)


# Import all tag detections from 2024

Detections <- dbGetQuery(conn = DB_conn, "SELECT  Detections.Detection_ID,
                                                  Detections.Upload_FK,
                                                  Detections.Scan_date_time,
                                                  Detections.Reader_code,
                                                  Detections.Antenna_Code,
                                                  Detections.Record_type,
                                                  Detections.Hex_tag_ID,
                                                  Detections.Dec_tag_ID,
                                                  Uploads.Array
                                            FROM Detections INNER JOIN Uploads ON Uploads.Upload_ID = Detections.Upload_FK
                                            WHERE YEAR(Detections.Scan_date_time) = '2024'")


# Import all fish tagged in 2024

Deployed <- dbGetQuery(conn = DB_conn, "SELECT  Deployed_tags.Tag_ID,
                                                Deployed_tags.Release_FK,
                                                Deployed_tags.Scan_date_time,
                                                Deployed_tags.Hex_tag_ID,
                                                Deployed_tags.Dec_tag_ID,
                                                Deployed_tags.Length,
                                                Deployed_tags.Notes,
                                                Releases.Release_date_time,
                                                Releases.Release_location,
                                                Releases.Capture_location,
                                                Releases.Capture_method,
                                                Releases.Species,
                                                Releases.Hatchery_status,
                                                Releases.Release_type
                                        FROM Deployed_tags INNER JOIN Releases ON Releases.Release_ID = Deployed_tags.Release_FK
                                        WHERE Year(Releases.Release_date_time) = '2024'")


# Disconnect from the database

dbDisconnect(DB_conn)

