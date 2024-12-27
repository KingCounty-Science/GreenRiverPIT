
## Connect to the PIT-tagging database and query data for the 2024 analysis ##


#### Load packages ####

if(!require(data.table)){install.packages('data.table'); library(data.table)} # Data manipulation package
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} # For data wrangling functions
if(!require(lubridate)){install.packages('lubridate'); library(lubridate)} # Easy date functions
if(!require(DBI)){install.packages('DBI')}; library(DBI) # For database connectivity
if(!require(dataRetrieval)){install.packages('dataRetrieval')}; library(dataRetrieval) # USGS data retrieval functions
if(!require(ggplot2)){install.packages('ggplot2')}; library(ggplot2) # For pretty plotting
if(!require(marked)){install.packages('marked')}; library(marked) # For mark-recapture modeling


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
                                    Duration = max(Scan_date_time) - min(Scan_date_time),
                                    Duration_days = round(as.numeric((max(Scan_date_time) - min(Scan_date_time))/86400), 3),
                                    Max_recap_length = max(Recapture_length)),
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

Merged[, Detected := ifelse(is.na(Merged$N), 0, 1)]


# Visualize the duration of time tagged Chinook spend in the Porter side channel, the Lower Russel backwater, 
# and the Duwamish People's park

Duration <- Merged[Species == "Chinook" & Detected == 1, ]

unique(Duration$Array)

Duration <- Duration[Array %in% c("Porter Side Channel", "Lower Russel Backwater", "Duwamish People's Park"), ]

Duration.plot <- ggplot(data = Duration, mapping = aes(y = Duration_days, x = Array)) +
  geom_boxplot(fill = 'steelblue') +
  labs(x = "Off-channel array", y = "Residence time (days)") +
  theme_bw()

ggsave(filename = "Duration.tiff", plot = Duration.plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 4.0, units = "in", dpi = 400, compression = 'lzw')


# Calculate times between releases to detection at the different arrays

Travel_times <- Merged[Detected == 1 & Release_type == "Experimental", 
                       .(Travel_time = round(as.numeric((First - Release_date_time)/86400), 3)), 
                       by = .(Hex_tag_ID, Release_date_time, Species, Length, Release_location, Array)]


# Subset to releases at the reservoir, Palmer hatchery, and WDFW screw trap

unique(Travel_times$Release_location)

Travel_times <- Travel_times[Release_location %in% c("WDFW Screw Trap",
                                                     "Palmer Ponds Outlet", 
                                                     "Howard Hanson Reservoir"), ]


# Drop the single detection at the WDFW screw trap

Travel_times[, .N, by = Array]

Travel_times <- Travel_times[Array != "WDFW Screw Trap", ]


# Create a new array variable that merges the two barges and two TBIOS locations, then order the arrays from upstream to downstream

Travel_times[, Array := factor(Array)]; levels(Travel_times$Array)

Travel_times[, Array_combined := as.factor(case_match(Array, c("Lower Green Barge 1", "Lower Green Barge 2") ~ "Lower Green Barges",
                                          c("WDFW TBiOS Opposite Slip 4", "WDFW TBiOS Slip 4") ~ "Slip 4",
                                          .default = Array))]

levels(Travel_times$Array_combined)

Travel_times[, Array_combined := factor(Array_combined, levels = c("Porter Side Channel",
                                                                    "Lower Green Barges",
                                                                    "Lower Russel Backwater",
                                                                    "Duwamish People's Park",
                                                                    "Slip 4"),
                                                                    ordered = TRUE)]

levels(Travel_times$Array_combined)


# Plot travel times for Chinook and Coho

Travel_time_plot <- ggplot(data = Travel_times, mapping = aes(y = Travel_time, x = Array_combined)) +
                            geom_violin(fill = 'steelblue') +
                            facet_wrap(as.factor(Travel_times$Release_location)) +
                            labs(x = "Detection location", y = "Travel time (days)") +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "Travel_times.tiff", plot = Travel_time_plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 5.5, units = "in", dpi = 400, compression = 'lzw')


# Reshape the merged data into wide format

All_wide <- dcast(data = Merged, Hex_tag_ID + Dec_tag_ID + Release_FK + Release_location + Release_type + 
                    Release_date_time + Species + Hatchery_status + Length ~ Array, value.var = "Detected")


# Drop the NA variable

All_wide[, 'NA' := NULL]


# Convert the NAs within the different array variables to zeros

All_wide[is.na(`Porter Side Channel`), `Porter Side Channel` := 0]

All_wide[is.na(`Lower Russel Backwater`), `Lower Russel Backwater` := 0]

All_wide[is.na(`Lower Green Barge 1`), `Lower Green Barge 1` := 0]

All_wide[is.na(`Lower Green Barge 2`), `Lower Green Barge 2` := 0]

All_wide[is.na(`Duwamish People's Park`), `Duwamish People's Park` := 0]

All_wide[is.na(`WDFW Screw Trap`), `WDFW Screw Trap` := 0]

All_wide[is.na(`WDFW TBiOS Slip 4`), `WDFW TBiOS Slip 4` := 0]

All_wide[is.na(`WDFW TBiOS Opposite Slip 4`), `WDFW TBiOS Opposite Slip 4` := 0]


# Split wide data into experimental and efficiency fish

Experimental_wide <- All_wide[Release_type == "Experimental", ]

Efficiency_wide <- All_wide[Release_type == "Efficiency", ]


# Split the experimental release data into Chinook and Coho releases

Experimental_wide_chinook <- Experimental_wide[Species == "Chinook", ]

Experimental_wide_coho <- Experimental_wide[Species == "Coho", ]


#### Experiment with constructing CJS models #### (This is currently a mess)

data(dipper)
head(dipper)


cjs.m1 <- crm(dipper)
cjs.m1

cjs.m1 <- cjs.hessian(cjs.m1)

predict(cjs.m1, SE = TRUE,)

plogis(cjs.m1$results$beta$Phi)

plogis(cjs.m1$results$beta$p)

dipper.proc <- process.data(dipper, 
                            group = "sex")

dipper.proc$data

dipper.ddl <- make.design.data(dipper.proc)

# Outine formulas for each parameter
Phi.dot <- list(formula=~1)  # ~1 is always a constant (or single estimate)
Phi.sex <- list(formula=~sex) # This formula will have an intercept (for females) and an estimate for the difference between females and males
p.sex <- list(formula=~sex) # Be careful of case-sensitive names. Use the exact group column that was in data

# Make new model (using design data) with constant survival, but different detection probabilities between sexes
cjs.m2 <- crm(dipper.proc, 
              dipper.ddl,
              model.parameters = list(Phi = Phi.dot, 
                                      p = p.sex),
              accumulate = FALSE)
## 

cjs.m2

Test <- Experimental_wide_chinook[Release_location %in% c("Palmer Ponds Outlet", "WDFW Screw Trap"), 1:14]

Test <- Test[, c("Release_location", 
                 "Hatchery_status", 
                 "Length", 
                 "Release_date_time", 
                 "Porter Side Channel",
                 "Lower Russel Backwater",
                 "Lower Green Barge 1",
                 "Lower Green Barge 2",
                 "Duwamish People's Park")]

Test[, ch := as.character(paste0(`Porter Side Channel`, 
                                 `Lower Russel Backwater`, 
                                 `Lower Green Barge 1`,
                                 `Lower Green Barge 2`,
                                 `Duwamish People's Park`))]
names(Test)

Test[,c("Porter Side Channel", 
     "Lower Russel Backwater", 
     "Lower Green Barge 1",
     "Lower Green Barge 2",
     "Duwamish People's Park") := NULL]

Test$Release_location <- as.factor(Test$Release_location)

Test$Hatchery_status <- as.factor(Test$Hatchery_status)

Test[, c("Length", "Release_date_time") := NULL]

Test_process <- process.data(data = Test, model = "CJS", groups = c("Release_location", "Hatchery_status"), accumulate = FALSE)
warnings()

Test.ddl <- make.design.data(Test_process)

Phi.location <- list(formula ~ Release_location)
Phi.origin <- list(formula ~ Hatchery_status)
Phi.time <- list(formula ~ time)

p.location <- list(formula ~ Release_location)
p.origin <- list(formula ~ Hatchery_status)

cjs.test <- crm(Test_process, Test.ddl, model.parameters = list(Phi = Phi.time, p = p.location))
