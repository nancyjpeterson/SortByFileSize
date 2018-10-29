#Applying CoordinateCleaner to the 1_unzip folder, adding IUCN Redlist column, adding species synonyms column to and sending the cleaned up occurrences to 2_aggregate

library(CoordinateCleaner)
library(dplyr)
library(data.table)

args <- commandArgs(TRUE) # call an Rscript from the command line

arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]





# This function will get input data from the 1_unzip folder, apply the CoordinateCleaner filters,
# the output is the cleaned file (which I'm actually not going to write anywhere; I use it as the input for the very next function in this)
GBIF_Clean <- function(Infile, CleanedFolder, CoordBox){
  
  print(paste0("working on this file: ", Infile))
  
  #load in the Unzipped occurrence.txt files from the Unzipped 5 degree coordinate folders.
  occ <- fread(input = Infile, header = TRUE, sep = '\t')
  
  #filter columns and keep only these ones. now call it occ_cols because we are keeping the necessary columns
  occ_cols <- occ %>% select( gbifID, identifier, license, modified, references, rights, rightsHolder, institutionCode,   collectionCode, datasetName, basisOfRecord, occurrenceID, catalogNumber, individualCount, eventDate, year, month, day, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, identificationID,	taxonID,	acceptedNameUsageID, scientificName, kingdom,	phylum,	class,	order, family, genus,	specificEpithet, taxonRank,	datasetKey,	publishingCountry, taxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, speciesKey, species, genericName, protocol, lastParsed, lastCrawled, repatriated)
  
  #filter for only the licenses we can use. Keeps CC_BY_4_0 CC_BY_NC_4_0 CC0_1_0. Gets rid of "UNSPECIFIED" and "UNSUPPORTED" 
  occ_clean_03 <- occ_cols[((occ_cols$license == "CC_BY_NC_4_0") | (occ_cols$license == "CC_BY_4_0")) | occ_cols$license == "CC0_1_0", ]
  
  
  # Clean up each occurrence.txt for bad coordinates. This piped step-by-step command is really simple and inspired by https://github.com/azizka/CoordinateCleaner tutorial command.
  # Note: I didn't use the built-in CleanCoordinates() command because it was really buggy.
  Cleanocc <- occ_clean_03 %>%
    
    # tests if its a valid coordinate value. According to the documentation this must be done first.
    cc_val(lat = "decimalLatitude" , lon = "decimalLongitude") %>% 
    
    # Flag Coordinates in Vicinity of Country Capitals.
    cc_cap(lat = "decimalLatitude" , lon =  "decimalLongitude" , buffer = 0.01) %>%
    
    # Flag Coordinates in Vicinity of Country and Province Centroids
    cc_cen(lat = "decimalLatitude" , lon = "decimalLongitude", buffer = .01) %>% 
    
    # Flag coordinates outside their reported country
    # cc_coun(lon = "decimalLongitude", lat = "decimalLatitude", iso3 = "countrycode", value = "clean", ref = NULL, verbose = TRUE) %>%
    
    # Flag duplicated records
    # cc_dupl(lon = "decimalLongitude", lat = "decimalLatitude" , species = "species", additions = NULL, value = "clean", verbose = TRUE) %>% 
    
    # Flag Records with Identical lat/lon
    cc_equ(lon = "decimalLongitude", lat = "decimalLatitude") %>%
    
    # Flag Records Assigned to GBIF Headquarters
    cc_gbif(lon = "decimalLongitude", lat = "decimalLatitude") %>%
    
    # Flag Records in the Vicinity of Biodiversity Institutions
    cc_inst(lon = "decimalLongitude", lat = "decimalLatitude" , buffer = 0.001) %>%
    
    # Flag Geographic Outliers in Species Distributions. We can't run this now bc we've split things up into 5 degree tiles
    # cc_outl(lat = "decimalLatitude" , lon = "decimalLongitude" , species = "species", method = "quantile", mltpl = 3, tdi = 1000, value = "clean", verbose = TRUE) %>%
    
    # Flag Non-terrestrial Coordinates
    cc_sea(lat = "decimalLatitude" , lon = "decimalLongitude") %>%
    
    # Flags records with either zero longitude or latitude and a radius around the point at zero longitude and zero latitude.
    cc_zero(lat = "decimalLatitude" , lon = "decimalLongitude" , buffer = 0.1) 
  
  # %>% 
  
  # Flags records in urban centers. I didn't run this bc we want urban data
  # cc_urb(lat = "decimalLatitude" , lon = "decimalLongitude") %>%
  
  # Flag Datasets with a Degree Conversion Error. It tests dataset level error so I didn't test it. Plus GBIF is all standardized in decimal degrees and WGS84
  # dc_ddmm() %>%
  
  #  Flags Datasets with Rasterized Coordinates
  # dc_round()
  
  #keep the file in memory so I can use this output as the input for my next function
  return(Cleanocc)
}

#Now execute the function and save the output as a variable. The resulting variable (Output_of_cleaning_function) will be my input into my next function

Output_of_cleaning_function <- GBIF_Clean(Infile = arg1, CleanedFolder = arg2, CoordBox = arg3)

# In this same job I am adding the IUCN Redlist status column for every occurrence and the species synonym
# columns ("Synonym.genus", "Synonym.species", "Synonym.infra.type", "Synonym.infrarank.name") to every occurrence.
# I'm running these two functions in the same R-script because I don't want to create a bunch of intermediate
# Monsoon outputs that take up a ton of space

# read in the IUCN csv from its file location. This contains the species, their redlist status, and taxonomic synonyms.
IUCN_and_synonyms <- read.csv('/projects/above_gedi/biodiv_data/gedi_global/0_raw/Z_GLOBAL/IUCN_data_info/data-1539596906885.csv', sep = ',')

# Function to add the species IUCN status and taxonomic synonyms. Uses the csv with 120,000 species provided by the IUCN
add_IUCN_synonym <- function(occurrence_csv,IUCN_data,CleanedFolder, CoordBox){
  #Use the match() function to key IUCN data and my occurrence data.
  #The proper match columns are: IUCN$Binomial and occurrence$species (the species columns of my occurrence data actually has the genus and species names). Then index into the Red.List.Category values and add those to my occurrence csv in the new column "IUCN.Status"
  occurrence_csv$IUCN.Status <- IUCN_data$Red.List.Category[match(occurrence_csv$species, IUCN_data$Binomial)]
  
  #Use the match() function in the same way as above, but this time I'm adding the species synonym columns.
  #(I could have these steps altogether with adding the Red.List.Category column but they serve different data analysis purposes so I'm keeping them separate)
  occurrence_csv <- cbind(occurrence_csv,IUCN_data[match(occurrence_csv$species, IUCN_data$Binomial),c("Synonym.genus", "Synonym.species", "Synonym.infra.type", "Synonym.infrarank.name")])
  
  # Defining the output file name so that each one is unique and self-labeled
  Cleaned_file_name <- paste0(CleanedFolder, CoordBox, "_cleanocc.csv")
  
  #write the file
  fwrite(file = Cleaned_file_name, x = occurrence_csv, row.names = FALSE)
}

#Now call the IUCN_syninym function
add_IUCN_synonym(occurrence_csv = Output_of_cleaning_function, IUCN_data = IUCN_and_synonyms,arg2,arg3)
