

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

DB_conn <- DBI::dbConnect(odbc::odbc(),
                     .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                 DBQ=P:/GregersenC/WRIA9/PIT_STUDIES/PIT_Database/Backend/PIT_tag_database_v1.0_be.accdb")

dbListTables(DB_conn)

dbListFields(conn = DB_conn, name = "Detections")
dbListFields(conn = DB_conn, name = "Deployed_tags")
dbListFields(conn = DB_conn, name = "Release_locations")
dbListFields(conn = DB_conn, name = "Releases")
dbListFields(conn = DB_conn, name = "Arrays")
dbListFields(conn = DB_conn, name = "Antennas")


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
                                        WHERE YEAR(Releases.Release_date_time) = '2024'")


# Disconnect from the database

dbDisconnect(DB_conn)


#### Organize the deployed tag data and detection data ####

# Convert detections to a data.table and examine these data

Detections <- data.table(Detections)

Detections[, .N, by = .(Array, Record_type)]

length(unique(Detections$Hex_tag_ID))
length(unique(Detections$Dec_tag_ID))


# Summarize detections by tag and array

Detections_summary <- Detections[, .(.N, 
                                     First = min(Scan_date_time), 
                                     Last = max(Scan_date_time),
                                     Duration = (max(Scan_date_time) - min(Scan_date_time))),
                                 keyby = .(Hex_tag_ID, Array)]


# Convert the deployed tags to a data.table and examine these data

Deployed <- data.table(Deployed)

Deployed[, .N, by = .(Release_location, Release_date_time, Species, Hatchery_status)]

length(unique(Deployed$Hex_tag_ID))
length(unique(Deployed$Dec_tag_ID))


# Join deployed tags to detections

Merged <- merge(x = Deployed, y = Detections_summary, by = "Hex_tag_ID", all.x = TRUE)


# Add a variable specifying if an individual tag was detected at an array

Merged$Detected <- ifelse(is.na(Merged$N), 0, 1)


# Reshape data into wide format

All_wide <- dcast(data = Merged, Hex_tag_ID + Dec_tag_ID + Release_FK + Release_location + Release_type + 
                    Release_date_time + Species + Hatchery_status + Length ~ Array,
                  value.var = "Detected")


# Drop the NA variable

All_wide[, 'NA' := NULL]


# Convert the NAs within the different array variables to zeros

All_wide[is.na(`Porter Side Channel`), `Porter Side Channel` := 0]

All_wide[is.na(`Lower Russel Backwater`), `Lower Russel Backwater` := 0]


# Split wide data into experimental and efficiency fish

Experimental_wide <- All_wide[Release_type == "Experimental", ]

Efficiency_wide <- All_wide[Release_type == "Efficiency", ]


