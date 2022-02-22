# Andrew Black
# andrewjblack0@gmail.com

# test change for new branch
# ctrl-shift-w to close all View tabs
# ctrl-/ to comment-out a section

# List out packages
	packages = c("ggplot2",
						"readxl",
						 "readr",
						 "tidyr",
						 "UpSetR",
						 "dplyr",
						 "purrr",
						 "stringr",
						 "ComplexUpset",
						 "apaTables",
						 "sos",
						 "imputeTS",
						 "ggmap",
						 "readtext",
						 "expss",
						 "ggsci",
						 "tmaptools",
						 "usmap",
						 "knitr",
						 "afex",
						 "stringr",
						 "ggthemes",
						 "scales",
						 "grid",
						 "reshape2",
						 "ggpubr")


	# Now load or install&load all
		package.check <- lapply(
		  packages,
		  FUN = function(x) {
		    if (!require(x, character.only = TRUE)) {
		      install.packages(x, dependencies = TRUE)
		      library(x, character.only = TRUE)
		    }
		  }
		)

# Set up colors for the colorblind
	scale_colour_discrete <- scale_colour_colorblind

# Navigate to the first file and open it
	setwd("C:/Users/Andrew/Desktop/UCMR")

# Get EPA data combined into one dataframe
	# Open .txt files provided in UCMR3 zip folder from EPA
		
		# UCMR3 All occurrence data
			UCMR3_allData <- read.table("UCMR3_All.txt", fill=TRUE, header=TRUE, quote="", sep="\t", encoding="UTF-8")
			#View(UCMR3_allData)
			unique(UCMR3_allData$State) # Wooooh, got all states and some territories!

			UCMR3_allData <- UCMR3_allData[!with(UCMR3_allData,is.na(MRL)& is.na(AnalyticalResultValue)& is.na(Contaminant)),] # remove rows where MRL is NA AND AnalyticalResultValue is NA, because this is not even a ND, there is no contaminant test reported
			
			UCMR3_allData$PWSID_facilityID <- paste(UCMR3_allData$PWSID, UCMR3_allData$FacilityID, sep="")

			length(unique(UCMR3_allData$PWSID_facilityID)) # 22,131 total unique facilities

			UCMR3_allData$PWSID_facilty_water <- paste(UCMR3_allData$PWSID, UCMR3_allData$FacilityWaterType, sep="")
			#View(UCMR3_allData)

		# UCMR3 disinfectant use data
			UCMR3_DT <- read.table("UCMR3_DT.txt", fill=TRUE, header=TRUE, quote="", sep="\t", encoding="UTF-8")
			#View(UCMR3_DT)
			UCMR3_DT$PWSID_facilityID <- paste(UCMR3_DT$PWSID, UCMR3_DT$FacilityID, sep="")
			# Remove the columns that are not needed, sample event code and collection date
			UCMR3_DT <- UCMR3_DT[, c("PWSID_facilityID", "Disinfectant.Type")]
			# Remove duplicate rows
			UCMR3_DT <- unique(UCMR3_DT)
			

			# this means that we must match up the disinfectant data based on BOTH PWSID and FacilityID, because a given PWSID may have multiple facilities under it, and the state-assigned facility ID has been proven not-unique (so, a PWSID from Kansas and Florida may have the same facility number 771, under unique PWSIDs)
			
			UCMR3_disinf <- left_join(UCMR3_allData, UCMR3_DT, by = "PWSID_facilityID")
			#View(UCMR3_disinf)

		# Get a nice list of the column names
			dput(as.character(names(UCMR3_disinf)))

	# Look at UCMR4 EPA data, get look up values for these PWSIDs treatment info
		# Open .txt files provided in UCMR3 zip folder from EPA
		UCMR4_HAA_AddtlDataElem <- read.delim("UCMR4_HAA_AddtlDataElem.txt")
		#View(UCMR4_HAA_AddtlDataElem)

		# make the column AdditionalDataElement into separate columns with Response below each
			UCMR4_HAA_AddtlDataElem_wide <- UCMR4_HAA_AddtlDataElem %>%
					  group_by(AdditionalDataElement) %>%
					  mutate(row = row_number()) %>%
					  tidyr::pivot_wider(names_from = AdditionalDataElement, values_from = Response) %>%
					  select(-row)
			#View(UCMR4_HAA_AddtlDataElem_wide)
		UCMR4_HAA_AddtlDataElem_wide$PWSID_facilityID <- paste(UCMR4_HAA_AddtlDataElem_wide$PWSID, UCMR4_HAA_AddtlDataElem_wide$FacilityID, sep="")


		# Make a table of treatment info by PWSID
			UCMR4_HAA_AddtlDataElem_wide_1 <- unique(subset(UCMR4_HAA_AddtlDataElem_wide, select = c("PWSID_facilityID", "TreatmentInformation")))
			UCMR4_HAA_AddtlDataElem_wide_1 <- na.omit(UCMR4_HAA_AddtlDataElem_wide_1)
			#View(UCMR4_HAA_AddtlDataElem_wide_1)
			PWSID_TreatmentInfo <- UCMR4_HAA_AddtlDataElem_wide_1 %>% 
			    group_by(PWSID_facilityID) %>% 
			    summarise(Treatment = paste(unique(TreatmentInformation), collapse = ', '))
			#View(PWSID_TreatmentInfo)

			# see how many of these from original UCMR4 set have biofiltration
			PWSID_TreatmentInfo_BF <- PWSID_TreatmentInfo
			PWSID_TreatmentInfo_BF$Biofiltration <- ifelse(is.na(PWSID_TreatmentInfo$Treatment), NA, grepl("BIO", PWSID_TreatmentInfo$Treatment))
			PWSID_TreatmentInfo_BF <- merge(x = PWSID_TreatmentInfo_BF, y = UCMR4_HAA_AddtlDataElem_wide[ , c("PWSID", "FacilityID", "PWSID_facilityID")], by.x = 'PWSID_facilityID', by.y = 'PWSID_facilityID')
			# Make it only unique rows
			PWSID_TreatmentInfo_BF <- unique(PWSID_TreatmentInfo_BF)
			#View(PWSID_TreatmentInfo_BF) # There are 7,326 unique facilities with treatment info from UCMR4

			# Of these, how many PWSIDs have different BF status within their included facilities? Filter out to just PWSID and Biofiltration and set to unique, then check for duplicate PWSIDs in that column
			unique_PWSID_BF <- unique(subset(PWSID_TreatmentInfo_BF, select = c("PWSID", "Biofiltration")))
			#View(unique_PWSID_BF) # 4,418 unique combinations of PWSID and Biofiltration. How many unique PWSIDs then?
			length(unique(unique_PWSID_BF$PWSID)) # 4,415 so thats only 3 differences. Thats nothing!

			# see how many of these from original UCMR4 set have GAC
			PWSID_TreatmentInfo_GAC <- PWSID_TreatmentInfo_BF
			PWSID_TreatmentInfo_GAC$GAC <- ifelse(is.na(PWSID_TreatmentInfo_BF$Treatment), NA, grepl("GAC", PWSID_TreatmentInfo_BF$Treatment))
			PWSID_TreatmentInfo_GAC <- merge(x = PWSID_TreatmentInfo_GAC, y = UCMR4_HAA_AddtlDataElem_wide[ , c("PWSID", "FacilityID", "PWSID_facilityID")], by.x = 'PWSID_facilityID', by.y = 'PWSID_facilityID')
			# Make it only unique rows
			PWSID_TreatmentInfo_GAC <- unique(PWSID_TreatmentInfo_GAC)
			#View(PWSID_TreatmentInfo_GAC) # There are 7,326 unique facilities with treatment info from UCMR4

			# Of these, how many PWSIDs have different GAC status within their included facilities? Filter out to just PWSID and GAC and set to unique, then check for duplicate PWSIDs in that column
			unique_PWSID_GAC <- unique(subset(PWSID_TreatmentInfo_GAC, select = c("PWSID.x", "GAC")))
			#View(unique_PWSID_GAC) # 4,433 unique combinations of PWSID and GAC. How many unique PWSIDs then?
			length(unique(unique_PWSID_GAC$PWSID)) # 4,415 so thats only 18 differences. Thats not much!


			# Okay, combine this info later based on PWSID and see how many NAs. Pick this up again further down in this script...
			treatment_data <- subset(PWSID_TreatmentInfo_GAC, select = c("PWSID.x", "FacilityID.x", "PWSID_facilityID", "Treatment", "Biofiltration", "GAC"))
			names(treatment_data)[names(treatment_data)=="PWSID.x"] <- "PWSID"
			names(treatment_data)[names(treatment_data)=="FacilityID.x"] <- "FacilityID"
			#View(treatment_data)



# Merge this new dataframes to original UCMR3 dataframe, first by PWSID_facilityID and count NAs
		UCMR3_w_treatment_data1 <- left_join(UCMR3_allData, treatment_data, by='PWSID_facilityID') # left_join keeps the "by" from the first listed dataframe, and fills in from the next dataframe to match up to PWSID_facilityIDs
		#View(UCMR3_w_treatment_data1)
		sum(is.na(UCMR3_w_treatment_data1$Biofiltration))
		sum(is.na(UCMR3_w_treatment_data1$GAC))
		

# Repeat by matching first PWSID then facility ID
		UCMR3_w_treatment_data2 <- left_join(UCMR3_allData, treatment_data, by= c('PWSID', 'FacilityID')) # left_join keeps the "by" from the first listed dataframe, and fills in from the next dataframe to match up to PWSID_facilityIDs
		#View(UCMR3_w_treatment_data2)
		sum(is.na(UCMR3_w_treatment_data2$Biofiltration))
		sum(is.na(UCMR3_w_treatment_data2$GAC))

# Repeat by matching just PWSID
		UCMR3_w_treatment_data3 <- left_join(UCMR3_allData, treatment_data, by= 'PWSID') # left_join keeps the "by" from the first listed dataframe, and fills in from the next dataframe to match up to PWSID_facilityIDs
		#View(UCMR3_w_treatment_data3)
		sum(is.na(UCMR3_w_treatment_data3$Biofiltration))
		sum(is.na(UCMR3_w_treatment_data3$GAC))

# This gave us 1,044,447 NA's for set 1 and 2, but only 254,102 NA's for set 3. Can we confirm that it is accurate to assign this info by PWSID, not facility ID? Yes, there were only 3 and 18 mismatches for BIO and GAC, respectively. So only 21 PWSIDs had different responses for these two within their included facilities. Lets go with set 3!

		final_data <- UCMR3_w_treatment_data3
		#View(final_data)


# Filter contaminant for 1,4-dioxane, strontium, PFOS, PFOA, and 1,2,3-trichloropropane
	final_data_regdet <- dplyr::filter(final_data,
										Contaminant == "1,4-dioxane" |
										Contaminant == "PFOS" |
										Contaminant == "PFOA" |
										Contaminant == "strontium" |
										Contaminant == "1,2,3-trichloropropane")
# Remove duplicate columns and rename some
		# Get a nice list of the column names
			dput(as.character(names(final_data_regdet)))
	final_data_regdet <- subset(final_data_regdet, select = c("PWSID", "PWSName", "Size", "FacilityID.x", "FacilityName","FacilityWaterType", "SamplePointID", "SamplePointName", "SamplePointType", "AssociatedFacilityID", "AssociatedSamplePointID", "CollectionDate", "SampleID", "Contaminant", "MRL", "MethodID", "AnalyticalResultsSign", "AnalyticalResultValue", "SampleEventCode", "MonitoringRequirement", "Region", "State", "PWSID_facilityID.x", "PWSID_facilty_water", "Treatment", "Biofiltration", "GAC"))

	names(final_data_regdet)[names(final_data_regdet)=="FacilityID.x"] <- "FacilityID"
	names(final_data_regdet)[names(final_data_regdet)=="PWSID_facilityID.x"] <- "PWSID_facilityID"
	

	View(final_data_regdet)
	# remove na values and send to new dataframe
	final_data_regdet_detects <- final_data_regdet[!is.na(final_data_regdet$AnalyticalResultValue),]
	View(final_data_regdet_detects)

	final_data_regdet_detects_dioxane <- filter(final_data_regdet_detects, Contaminant == "1,4-Dioxane")
	final_data_regdet_detects_trichloropropane <-filter(final_data_regdet_detects, Contaminant == "1,2,3-trichloropropane")
	final_data_regdet_detects_PFOS <- filter(final_data_regdet_detects, Contaminant == "PFOS")
	final_data_regdet_detects_PFOA <- filter(final_data_regdet_detects, Contaminant == "PFOA")


# Take 95th percentile of AnalyticalResultValue for each PWSID_facilityID, and include particular columns
				
			# create the 95th percentile function
				percentile95 <- function (x){
					quantile(x, probs = 0.95)
				}

			
			# create the new data sets

			# dioxane	

				s95th_dioxane <- aggregate(x = final_data_regdet_detects_dioxane$AnalyticalResultValue,     # Specify data column
				          by = list(final_data_regdet_detects_dioxane$PWSID_facilityID),                          # Specify group indicator
				          FUN = percentile95)                                            # Specify function (i.e. mean)
				colnames(s95th_dioxane) <- c("PWSID_facilityID", "s95thAnalyticalResultValue_dioxane")
				View(s95th_dioxane)

			# PFOS	

				s95th_PFOS <- aggregate(x = final_data_regdet_detects_PFOS$AnalyticalResultValue,     # Specify data column
				          by = list(final_data_regdet_detects_PFOS$PWSID_facilityID),                          # Specify group indicator
				          FUN = percentile95)                                            # Specify function (i.e. mean)
				colnames(s95th_PFOS) <- c("PWSID_facilityID", "s95thAnalyticalResultValue_PFOS")
				View(s95th_PFOS)

			# PFOA

				s95th_PFOA <- aggregate(x = final_data_regdet_detects_PFOA$AnalyticalResultValue,     # Specify data column
				          by = list(final_data_regdet_detects_PFOA$PWSID_facilityID),                          # Specify group indicator
				          FUN = percentile95)                                            # Specify function (i.e. mean)
				colnames(s95th_PFOA) <- c("PWSID_facilityID", "s95thAnalyticalResultValue_PFOA")
				View(s95th_PFOA)

			# trichloropropane

				s95th_trichloropropane <- aggregate(x = final_data_regdet_detects_trichloropropane$AnalyticalResultValue,     # Specify data column
				          by = list(final_data_regdet_detects_trichloropropane$PWSID_facilityID),                          # Specify group indicator
				          FUN = percentile95)                                            # Specify function (i.e. mean)
				colnames(s95th_trichloropropane) <- c("PWSID_facilityID", "s95thAnalyticalResultValue_trichloropropane")
				View(s95th_trichloropropane)

	

			# how many over HAL for:
				# dioxane
					dioxane_exceeds_10_6 <- sum(s95th_dioxane$s95thAnalyticalResultValue_dioxane > 0.48)
					dioxane_exceeds_10_6
					dioxane_exceeds_10_4 <-sum(s95th_dioxane$s95thAnalyticalResultValue_dioxane > 48)
					dioxane_exceeds_10_4

					dioxane_average_conc <- mean(final_data_regdet_detects_dioxane$AnalyticalResultValue) * 1000
					dioxane_average_conc

					dioxane_sd_conc <- sd(final_data_regdet_detects_dioxane$AnalyticalResultValue) * 1000
					dioxane_sd_conc
					

				# PFOS
						PFOS_exceeds_HAL <-sum(s95th_PFOS$s95thAnalyticalResultValue_PFOS > 0.07)
						PFOS_exceeds_HAL

						PFOS_average_conc <- mean(final_data_regdet_detects_PFOS$AnalyticalResultValue) * 1000
						PFOS_average_conc

						PFOS_sd_conc <- sd(final_data_regdet_detects_PFOS$AnalyticalResultValue) * 1000
						PFOS_sd_conc

				# PFOA
						PFOA_exceeds_HAL <-sum(s95th_PFOA$s95thAnalyticalResultValue_PFOA > 0.07)
						PFOA_exceeds_HAL

						PFOA_average_conc <- mean(final_data_regdet_detects_PFOA$AnalyticalResultValue) * 1000
						PFOA_average_conc

						PFOA_sd_conc <- sd(final_data_regdet_detects_PFOA$AnalyticalResultValue) * 1000
						PFOA_sd_conc

				# TCP
						trichloropropane_exceeds_10_6 <- sum(s95th_trichloropropane$s95thAnalyticalResultValue_trichloropropane > 0.0046)
						trichloropropane_exceeds_10_6
						trichloropropane_exceeds_10_4 <-sum(s95th_trichloropropane$s95thAnalyticalResultValue_trichloropropane > 0.046)
						trichloropropane_exceeds_10_4

						trichloropropane_average_conc <- mean(final_data_regdet_detects_trichloropropane$AnalyticalResultValue) * 1000
						trichloropropane_average_conc

						trichloropropane_sd_conc <- sd(final_data_regdet_detects_trichloropropane$AnalyticalResultValue) * 1000
						trichloropropane_sd_conc




