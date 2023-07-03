##----------------A code for extracting and cleaning GBIF data----------------##

## Further information on GBIF data at "https://www.gbif.org/".

##============================================================================##
## Install required packages
##============================================================================##
if(!require(rgbif)){install.packages("rgbif");library(rgbif)}
if(!require(sf)){install.packages("sf");library(sf)}
if(!require(raster)){install.packages("raster");library(raster)}
if(!require(CoordinateCleaner)){install.packages("CoordinateCleaner");library(CoordinateCleaner)}

##============================================================================##
## Set the directory
##============================================================================##
setwd("~/Library/CloudStorage/OneDrive-TheLandbankingGroupGmbH/SDM")

##============================================================================##
## Import the data on list of species
##============================================================================##
species_list <- c(scan(file="GBIF/species_list_terr.txt", what = "", sep = "\n"))

##============================================================================##
## GBIF data acquisition
##============================================================================##
d <- occ_data(scientificName = species_list,
                    hasCoordinate = TRUE,
                    continent = "africa",
                    occurrenceStatus = "PRESENT",
                    basisOfRecord = "HUMAN_OBSERVATION;MATERIAL_CITATION;MATERIAL_SAMPLE;
                    MACHINE_OBSERVATION;OBSERVATION;OCCURRENCE",
                    hasGeospatialIssue = "FALSE",
                    year = "2016,2023",
                    limit = 10000,
                    coordinateUncertaintyInMeters = "0,50000"
                    )

df = list()
for (i in 1:length(d)){
  df[[i]] =cbind.data.frame(d[[i]][["data"]][["gbifID"]],
                            d[[i]][["data"]][["decimalLongitude"]],
                            d[[i]][["data"]][["decimalLatitude"]],
                            d[[i]][["data"]][["scientificName"]],
                            d[[i]][["data"]][["year"]],
                            d[[i]][["data"]][["month"]],
                            d[[i]][["data"]][["country"]],
                            d[[i]][["data"]][["collectionCode"]],
                            d[[i]][["data"]][["coordinateUncertaintyInMeters"]])
  
  names(df[[i]]) = c("ID","Longitude", "Latitude", "scientificName", 
                     "referenceYear", "referenceMonth", "country",
                     "collectionCode","locationUncertaintyMeters")
  
  # Identify and remove invalid lat/lon coordinates
  df[[i]] = cc_val(df[[i]], lon = "Longitude", lat = "Latitude",
                   value = "clean", verbose = TRUE)
  
  # Identify and remove duplicate coordinates
  df[[i]] =cc_dupl(df[[i]], lon = "Longitude", lat = "Latitude", 
                   species = "scientificName",
                   additions = NULL, value = "clean", verbose = TRUE)
  
  # Identify and remove coordinates outside the reference landmass
  df[[i]] = cc_sea(df[[i]], lon = "Longitude", lat = "Latitude",
                   value = "clean", verbose = TRUE)
  
  downloadDate = Sys.Date()
  df[[i]]["downloadDate"] = downloadDate
  df[[i]]["presence"] = 1
  
  # Printing the number of species before and after clean-up.
  print(species_list[[i]])
  print(paste("Before Cleanup", d[[i]][["meta"]][["count"]]))
  print(paste("After Cleanup", nrow(df[[i]])))
  
  # Assigning projection and writing spatial files
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  df[[i]] <- st_as_sf(x=df[[i]],
                      coords = c("Longitude", "Latitude"),
                      crs = projcrs)
  
  # Saving .gpkg files in local computer
  species_list = gsub(" ", "_", species_list) 
  sf::st_write(df[[i]],
               dsn = paste0(getwd(), "/GBIF/GBIF_output/", species_list[[i]], ".gpkg"),
               delete_layer =TRUE)
  
}

## To save as shapefiles
#coordinates(df[[i]])=~Longitude+Latitude
#proj4string(df[[i]])<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#raster::shapefile(df[[i]],
#                  filename=paste0("GBIF/GBIF_output_shp/", species_list[[i]], ".shp"),
#                  overwrite=TRUE)

##==================================THE END===================================##

