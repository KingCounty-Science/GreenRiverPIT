
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

setDT(Detections); str(Detections)

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

setDT(Deployed); str(Deployed)

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


# Add a variable that indicates if a tag was detected in a yes/no format

Merged[, Detected_logical := factor(case_match(Detected, 1 ~ "Yes", 0 ~ "No"))]


# Add a tag-type variable

Merged[, Tag_type := substr(Hex_tag_ID, start = 1, stop = 3)]

unique(Merged$Tag_type)

Merged[, Tag_type := factor(case_match(Tag_type, "3D6" ~ "9mm", "3DD" ~ "12mm", "3DE" ~ "8mm"))]

Merged[, .N, by = Tag_type]


#### Visualize the duration of time tagged Chinook spend at Porter, Lower Russel, and Duwamish People's Park ####

Duration <- Merged[Species == "Chinook" & Detected == 1, ]

unique(Duration$Array)

Duration <- Duration[Array %in% c("Porter Side Channel", "Lower Russel Backwater", "Duwamish People's Park"), ]

Duration.plot <- ggplot(data = Duration, mapping = aes(y = Duration_days, x = Array)) +
                        geom_boxplot(fill = 'steelblue') +
                        labs(x = "Off-channel array", y = "Residence time (days)") +
                        coord_cartesian(ylim = c(0,45)) + # Note this line omits the fish that died at DPP
                        theme_bw() 

ggsave(filename = "Duration.tiff", plot = Duration.plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 4.0, units = "in", dpi = 400, compression = 'lzw')


#### Visualize travel times between releases to detection at the different arrays ####

Travel_times <- Merged[Detected == 1, .(Travel_time = round(as.numeric((First - Release_date_time)/86400), 3)), 
                       by = .(Hex_tag_ID, Release_date_time, Species, Length, Release_location, Array)]


# Subset to releases at the reservoir, Palmer hatchery, WDFW screw trap, and Tukwilla pedestrian bridge,
# then order the release locations from upstream to downstream

Travel_times[, .N, by = Release_location]

Travel_times <- Travel_times[Release_location %in% c("WDFW Screw Trap",
                                                     "Palmer Ponds Outlet", 
                                                     "Howard Hanson Reservoir",
                                                     "Tukwila Pedestrian Bridge"), ]

Travel_times[, Release_location := factor(Release_location, levels = c("Howard Hanson Reservoir",
                                                                       "Palmer Ponds Outlet",
                                                                       "WDFW Screw Trap",
                                                                       "Tukwila Pedestrian Bridge"),
                                                                        ordered = TRUE)]

levels(Travel_times$Release_location)


# Drop the single detection at the WDFW screw trap

Travel_times[, .N, by = Array]

Travel_times <- Travel_times[Array != "WDFW Screw Trap", ]


# Create a new array variable that merges the two barges and two TBIOS locations, then order the location from upstream to downstream

Travel_times[, Array := factor(Array)]; levels(Travel_times$Array)

Travel_times[, Array_combined := as.factor(case_match(Array, c("Lower Green Barge 1", "Lower Green Barge 2") ~ "Green Barges",
                                                             c("WDFW TBiOS Opposite Slip 4", "WDFW TBiOS Slip 4") ~ "Slip 4",
                                                             "Lower Russel Backwater" ~ "Lower Russel",
                                                             "Duwamish People's Park" ~ "People's Park",
                                                             "Porter Side Channel" ~ "Porter",
                                                             .default = Array))]

levels(Travel_times$Array_combined)
Travel_times[, .N, by = Array_combined]

Travel_times[, Array_combined := factor(Array_combined, levels = c("Porter",
                                                                    "Green Barges",
                                                                    "Lower Russel",
                                                                    "People's Park",
                                                                    "Slip 4"),
                                                                    ordered = TRUE)]

levels(Travel_times$Array_combined)
Travel_times[, .N, keyby = Array_combined]


# Plot travel times for Chinook and Coho

Travel_time_plot <- ggplot(data = Travel_times, mapping = aes(y = Travel_time, x = Array_combined)) +
                            geom_violin(fill = 'steelblue') +
                            facet_wrap(as.factor(Travel_times$Release_location)) +
                            labs(x = "Detection location", y = "Travel time (days)") +
                            theme_bw() +
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave(filename = "Travel_times.tiff", plot = Travel_time_plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 5.5, units = "in", dpi = 400, compression = 'lzw')


Travel_time_release_timing_plot <- ggplot(data = Travel_times, mapping = aes(y = Travel_time, x = Release_date_time)) +
                                          geom_point(aes(col = Array_combined), size = 1) +
                                          facet_wrap(as.factor(Travel_times$Release_location)) +
                                          labs(x = "Release timing", y = "Travel time (days)", col = "Detection location") +
                                          theme_bw()

ggsave(filename = "Travel_times_release_timing.tiff", plot = Travel_time_release_timing_plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 4.0, units = "in", dpi = 400, compression = 'lzw')


Travel_time_length_plot <- ggplot(data = Travel_times, mapping = aes(y = Travel_time, x = Length)) +
                                          geom_point(aes(col = Array_combined), size = 1) +
                                          facet_wrap(as.factor(Travel_times$Release_location)) +
                                          labs(x = "Fork length (mm)", y = "Travel time (days)", col = "Detection location") +
                                          theme_bw()

ggsave(filename = "Travel_times_length.tiff", plot = Travel_time_length_plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 4.0, units = "in", dpi = 400, compression = 'lzw')


#### Compare lengths and release timing of Chinook that were detected and not detected at the barges ####

# Subset to releases from Palmer and the screw trap

Variable_comp <- Merged[Release_location %in% c("WDFW Screw Trap", "Palmer Ponds Outlet"), ]


# Subset to only barge detections and non-detects

Variable_comp <- Variable_comp[Array %in% c("Lower Green Barge 1", "Lower Green Barge 2", NA), ]


# Plot fork lengths vs. detection status

Length_comp <- ggplot(data = Variable_comp, mapping = aes(y = Length, x = Detected_logical)) +
                      geom_violin(fill = 'steelblue') +
                      facet_wrap(vars(Tag_type, Release_location)) +
                      labs(x = "Detected at the barges?", y = "Fork length (mm)") +
                      theme_bw()

ggsave(filename = "Length_comparison.tiff", plot = Length_comp, device = "tiff", path = "R/Output",
       width = 6.5, height = 5.5, units = "in", dpi = 400, compression = 'lzw')


# Plot deployment timing vs. detection status

Date_comp <- ggplot(data = Variable_comp, mapping = aes(y = Release_date_time, x = Detected_logical)) +
                    geom_violin(fill = 'steelblue') +
                    facet_wrap(vars(Tag_type, Release_location)) +
                    labs(x = "Detected at the barges?", y = "Release timing") +
                    theme_bw()

ggsave(filename = "Date_comparison.tiff", plot = Date_comp, device = "tiff", path = "R/Output",
       width = 6.5, height = 5.5, units = "in", dpi = 400, compression = 'lzw')


#### Load Green River flow data ####

USGS_flow <- readNWISdv(siteNumbers = "12113000",
                        parameterCd = "00060",
                        startDate = "2024-03-01",
                        endDate = "2024-07-05",
                        statCd = "00003")

setDT(USGS_flow)

names(USGS_flow)

names(USGS_flow) <- c("Agency", "Site_ID", "Date", "Mean_flow", "Status")


# Summarize the number of fish deployed for each release location and release date

Deployed_summary <- Deployed[, .N, by = .(Release_date = as_date(Release_date_time), Release_location, Species)]


# Plot flow data and number of fish released on different dates

Flow_plot <- ggplot(data = Deployed_summary, mapping = aes(y = N, x = Release_date)) +
                    geom_point(aes(col = Release_location)) +
                    geom_line(data = USGS_flow, aes(y = Mean_flow-450, x = Date), col = 'steelblue', linewidth = 1) +
                    labs(x = "Date", col = "Release location") +
                    scale_y_continuous(name = "Number released", 
                    sec.axis = sec_axis(~.+450, name = "Green River flow @ Auburn (cfs)")) +
                    theme_bw()
  
ggsave(filename = "Flow_plot.tiff", plot = Flow_plot, device = "tiff", path = "R/Output",
       width = 6.5, height = 4.0, units = "in", dpi = 400, compression = 'lzw')


# Calculate a forward-looking 10-day rolling average of daily mean flows

USGS_flow[, Ten_day_mean := frollmean(x = Mean_flow, n = 10, fill = NA, align = "left", algo = "exact")]


#### Reshape the merged data into a format for mark re-capture modeling ####

# Omit detections at the WDFW screw trap and slip 4

Merged[, .N, by = Array]

For_modeling <- Merged[Array %in% c("Porter Side Channel", 
                                "Lower Russel Backwater",
                                "Lower Green Barge 1",
                                "Lower Green Barge 2",
                                "Duwamish People's Park",
                                NA), ]


# Order the detection arrays from upstream to downstream

For_modeling[, Array := factor(Array, levels = c("Porter Side Channel", 
                                             "Lower Russel Backwater",
                                             "Lower Green Barge 1",
                                             "Lower Green Barge 2",
                                             "Duwamish People's Park"), 
                                              ordered = TRUE)]

levels(For_modeling$Array)


# Drop 8mm tags

For_modeling <- For_modeling[Tag_type != "8mm", ]


# Create a day-of-the-year release variable

For_modeling[, DOY := yday(Release_date_time)]


# Convert data to wide-format

All_wide <- dcast(data = For_modeling, Hex_tag_ID + Dec_tag_ID + Tag_type + Release_FK + Release_location + Release_type + 
                    Release_date_time + DOY + Species + Hatchery_status + Length ~ Array, value.var = "Detected")

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


# Split the experimental release data into Chinook and Coho releases

Experimental_wide_chinook <- Experimental_wide[Species == "Chinook", ]

Experimental_wide_coho <- Experimental_wide[Species == "Coho", ]

str(Experimental_wide_chinook)

#### Experiment with constructing CJS models (This is currently a mess) ####

data(dipper)
head(dipper)
str(dipper)

cjs.m1 <- crm(dipper)
cjs.m1

cjs.m1 <- cjs.hessian(cjs.m1)

cjs.m1

predict(cjs.m1, SE = TRUE,)

plogis(cjs.m1$results$beta$Phi)

plogis(cjs.m1$results$beta$p)

dipper.proc <- process.data(dipper, 
                            group = "sex")

dipper.proc$data

?process.data

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


Experimental_wide_chinook[, ch := as.character(paste0(`Porter Side Channel`, 
                                                      `Lower Russel Backwater`, 
                                                      `Lower Green Barge 1`,
                                                      `Lower Green Barge 2`,
                                                      `Duwamish People's Park`))]

Test <- with(data = Experimental_wide_chinook, data.frame(ch, Length, DOY, Tag_type, Hatchery_status, Release_location))

Test$Hatchery_status <- factor(Test$Hatchery_status)

Test$Release_location <- factor(Test$Release_location)

str(Test)

Test <- Test[, c("Hatchery_status", 
                 "Length", 
                 "Release_date_time", 
                 "Porter Side Channel",
                 "Lower Russel Backwater",
                 "Lower Green Barge 1",
                 "Lower Green Barge 2",
                 "Duwamish People's Park")]


?data.frame
Test2[, ch := as.character(paste0(`Porter Side Channel`, 
                                 `Lower Russel Backwater`, 
                                 `Lower Green Barge 1`,
                                 `Lower Green Barge 2`,
                                 `Duwamish People's Park`))]
names(Test2)

str(Test2)

Test[,c("Porter Side Channel", 
     "Lower Russel Backwater", 
     "Lower Green Barge 1",
     "Lower Green Barge 2",
     "Duwamish People's Park") := NULL]

#Test$Release_location <- as.factor(Test$Release_location)

Test$Hatchery_status <- as.factor(Test$Hatchery_status)

Test <- Test[!(is.na(Length)), ]

Test <- Test[, c(1,4)]
Test2 <- (as.data.frame(Test))

str(Test)
Test_process <- process.data(data = Test, model = "CJS")
warnings()
warnings()

Test_process$data

Test.ddl <- make.design.data(Test_process)

Test.ddl$design.parameters

Phi.location <- list(formula ~ Release_location)
Phi.origin <- list(formula ~ Hatchery_status)
Phi.time <- list(formula ~ time)

p.location <- list(formula ~ Release_location)
p.origin <- list(formula ~ Hatchery_status)

p.dot <- list(formula ~ 1)

cjs.test <- crm(Test_process, Test.ddl, model.parameters = list(Phi = Phi.origin, p = p.dot))

##

data(dipper)
str(dipper)
# Add a dummy weight field which are random values from 1 to 10
  set.seed(123)
dipper$weight=round(runif(nrow(dipper),0,9),0)+1

# Add Flood covariate
Flood=matrix(rep(c(0,1,1,0,0,0),each=nrow(dipper)),ncol=6)
colnames(Flood)=paste("Flood",1:6,sep="")
dipper=cbind(dipper,Flood)

# Add td covariate, but exclude first release as a capture
  # splitCH and process.ch are functions in the marked package
  td=splitCH(dipper$ch)
  
td=td[,1:6]
releaseocc=process.ch(dipper$ch)$first

releaseocc=cbind(1:length(releaseocc),releaseocc)
releaseocc=releaseocc[releaseocc[,2]<nchar(dipper$ch[1]),]
td[releaseocc]=0
colnames(td)=paste("td",2:7,sep="")
dipper=cbind(dipper,td)
# show names
  names(dipper)
  
  # Process data
    dipper.proc=process.data(dipper)
  
  # Create design data with static and time varying covariates
    design.Phi=list(static=c("weight"),time.varying=c("Flood"))
  design.p=list(static=c("sex"),time.varying=c("td"), age.bins=c(0,1,20))
  
  design.parameters=list(Phi=design.Phi, p=design.p)
  ddl=make.design.data(dipper.proc,parameters=design.parameters)
  names(ddl$Phi)
  names(ddl$p)

  
  Phi.sfw=list(formula=~Flood+weight)
  p.ast=list(formula=~age+sex+td)
  model=crm(dipper.proc,ddl,hessian=TRUE, model.parameters=list(Phi=Phi.sfw,p=p.ast))
  model
