
## Connect to the PIT-tagging database and query data for the 2024 analysis ##


#### Load packages ####

if(!require(data.table)){install.packages('data.table'); library(data.table)} # Data manipulation package
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} # For the 'case_match' function
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

dbListFields(conn = DB_conn, name = "Detections")
dbListFields(conn = DB_conn, name = "Deployed_tags")
dbListFields(conn = DB_conn, name = "Release_locations")
dbListFields(conn = DB_conn, name = "Releases")
dbListFields(conn = DB_conn, name = "Arrays")
dbListFields(conn = DB_conn, name = "Antennas")
dbListFields(conn = DB_conn, name = "Uploads")


# Import all tag detections from 2024

Detections <- dbGetQuery(conn = DB_conn, "SELECT  Detections.Detection_ID,
                                                  Detections.Upload_FK,
                                                  Detections.Scan_date_time,
                                                  Detections.Reader_code,
                                                  Detections.Antenna_Code,
                                                  Detections.Record_type,
                                                  Detections.Hex_tag_ID,
                                                  Detections.Dec_tag_ID,
                                                  Detections.Recapture_length,
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

Detections <- data.table(Detections); str(Detections)

Detections[, .N, by = .(Array, Record_type)]

length(unique(Detections$Hex_tag_ID))
length(unique(Detections$Dec_tag_ID))


# Summarize detections by tag and array, dropping pet tags and test tags

Detections_summary <- Detections[Record_type == "Tag", .(.N, 
                                    First = min(Scan_date_time), 
                                    Last = max(Scan_date_time),
                                    Duration = (max(Scan_date_time) - min(Scan_date_time))),
                                 keyby = .(Hex_tag_ID, Array, Upload_FK)]


# Convert the deployed tags to a data.table and examine these data

Deployed <- data.table(Deployed); str(Deployed)

Deployed[, .N, by = .(Release_FK, Release_location, Release_date_time, Species, Hatchery_status)]

length(unique(Deployed$Hex_tag_ID))
length(unique(Deployed$Dec_tag_ID))


# Add a character length vector for the decimal tag ID and export the table to a csv file

Deployed$N_char <- nchar(Deployed$Dec_tag_ID)

write.table(x = Deployed, file = "R/Output/Deployed_tags.csv", sep = ",", row.names = F)


# Join deployed tags to detections

Merged <- merge(x = Deployed, y = Detections_summary, by = "Hex_tag_ID", all.x = TRUE)


# Add a variable specifying if an individual tag was detected at an array

Merged$Detected <- ifelse(is.na(Merged$N), 0, 1)


# Calculate times between release to detection at the different arrays

Travel_times <- Merged[Detected == 1 & Release_type == "Experimental", 
                       .(Travel_time = round(as.numeric((First - Release_date_time)/86400), 3)), 
                       by = .(Hex_tag_ID, Release_date_time, Species, Length, Release_location, Array)]


# Subset to releases at the reservoir, Palmer hatchery, and WDFW screw trap

unique(Travel_times$Release_location)

Travel_times <- Travel_times[Release_location %in% c("WDFW Screw Trap",
                                                     "Palmer Ponds Outlet", 
                                                     "Howard Hanson Reservoir"),]


# Subset to detections at the Green barges, Porter side channel, Lower Russel backwater, Duwamish People's Park, and TBIOS recaptures

unique(Travel_times$Array)

Travel_times <- Travel_times[Array %in% c("Lower Green Barge 1", 
                                          "Lower Green Barge 2", 
                                          "Porter Side Channel", 
                                          "Duwamish People's Park",
                                          "WDFW TBiOS Opposite Slip 4", 
                                          "WDFW TBiOS Slip 4", 
                                          "Lower Russel Backwater"), ]


# Create a new array variable that merges the two barges and the two TBIOS locations

Travel_times[, Array := factor(Array)]; levels(Travel_times$Array)

Travel_times[, Array_combined := as.factor(case_match(Array, c("Lower Green Barge 1", "Lower Green Barge 2") ~ "Lower Green Barges",
                                          c("WDFW TBiOS Opposite Slip 4", "WDFW TBiOS Slip 4") ~ "Slip 4",
                                          .default = Array))]
levels(Travel_times$Array_combined)


# Order arrays from most upstream to most downstream

Travel_times[, Array_combined := factor(Array_combined, levels = c("Porter Side Channel",
                                                                    "Lower Green Barge",
                                                                    "Lower Russel Backwater",
                                                                    "Duwamish People's Park",
                                                                    "Slip 4"),
                                                                    ordered = TRUE)]
levels(Travel_times$Array_combined)


# Plot travel times for Chinook and Coho

ggplot(data = Travel_times, mapping = aes(y = Travel_time, x = Array_combined),) +
  geom_violin() +
  facet_wrap(as.factor(Travel_times$Release_location)) +
  labs(x = "Detection array", y = "Travel time (days)")


# Reshape data into wide format

All_wide <- dcast(data = Merged, Hex_tag_ID + Dec_tag_ID + Release_FK + Release_location + Release_type + 
                    Release_date_time + Species + Hatchery_status + Length ~ Array,
                  value.var = "Detected")


# Drop the NA variable

All_wide[, 'NA' := NULL]


# Convert the NAs within the different array variables to zeros

All_wide[is.na(`Porter Side Channel`), `Porter Side Channel` := 0]

All_wide[is.na(`Lower Russel Backwater`), `Lower Russel Backwater` := 0]

All_wide[is.na(`Lower Green Barge 1`), `Lower Green Barge 1` := 0]

All_wide[is.na(`Lower Green Barge 2`), `Lower Green Barge 2` := 0]

All_wide[is.na(`Duwamish People's Park`), `Duwamish People's Park` := 0]


# Split wide data into experimental and efficiency fish

Experimental_wide <- All_wide[Release_type == "Experimental", ]

Efficiency_wide <- All_wide[Release_type == "Efficiency", ]


