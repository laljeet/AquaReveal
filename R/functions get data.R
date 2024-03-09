
library(tidyverse)
library(usdarnass)

#' Clear and Reformat Data for Acres
#'
#' This function cleans and reformats the input dataset for analysis. It replaces commas in the `Value` field, converts the `Value` field from UTF-8 to ASCII to remove all characters except digits, and selects specific columns for the final dataset. It also creates a new `fips` column by concatenating the state and county codes.
#'
#' @param dat A data frame containing the columns `year`, `county_name`, `state_fips_code`, `county_ansi`, `asd_desc`, `asd_code`, `domain_desc`, `domaincat_desc`, and `Value`. The `Value` column should contain the area in acres, potentially formatted as a string with commas.
#'
#' @return A cleaned and reformatted data frame with columns for Year, County_Name, Region, Region_code, Domain, Size.bins, and Irr.Area.Acres. The `fips` column is also added, representing the concatenated state and county codes. Columns for state and county codes are removed from the output.
#'
#' @examples
#' # Assuming `df` is your dataset:
#' cleaned_data <- clear_data_acres(df)
#'
#' @export
#'
#' @importFrom dplyr select %>%
clear_data_acres <- function(dat) {
  dat$Value <- gsub(",", "", as.character(dat$Value))
  dat$Value <- as.numeric(iconv(dat$Value, 'utf-8', 'ascii', sub=''))
  dat <- dat %>%
    select(Year = year, County_Name = county_name, state_code = state_fips_code, County_Code = county_ansi, Region = asd_desc, Region_code = asd_code,
           Domain = domain_desc, Size.bins = domaincat_desc, Irr.Area.Acres = Value)
  dat$fips <- paste0(dat$state_code, dat$County_Code)
  dat <- dat %>%
    select(-c(state_code, County_Code))
  return(dat)
}

#' Clean Data for Operations
#'
#' This function cleans the agricultural land data related to the number of operations.
#' Similar to `clear_data_acres`, it processes the `Value` column by removing commas and converting from UTF-8 to ASCII,
#' selects and renames relevant columns for clarity, creates a `fips` column by combining state and county codes, and
#' drops the original state and county code columns.
#'
#' @param dat A data frame containing the columns: `Value`, `year`, `county_name`, `state_fips_code`, `county_ansi`, `asd_desc`, `asd_code`, `domain_desc`, and `domaincat_desc`.
#' @return A cleaned data frame with the columns: `Year`, `County_Name`, `Region`, `Region_code`, `Domain`, `Size.bins`, `Irr.Ops`, and `fips`.
#' @examples
#' # Assuming `dat` is your dataframe:
#' cleaned_ops_data <- clear_data_ops(dat)
#' @export
#'
clear_data_ops <- function(dat) {
  dat$Value <- gsub(",", "", as.character(dat$Value))
  dat$Value <- as.numeric(iconv(dat$Value, 'utf-8', 'ascii', sub=''))
  dat <- dat %>%
    select(Year = year, County_Name = county_name, state_code = state_fips_code, County_Code = county_ansi, Region = asd_desc, Region_code = asd_code,
           Domain = domain_desc, Size.bins = domaincat_desc, Irr.Ops = Value)
  dat$fips <- paste0(dat$state_code, dat$County_Code)
  dat <- dat %>%
    select(-c(state_code, County_Code))
  return(dat)
}

#' Fetch and Process NASS Data for a Given Year and State with API Key
#'
#' This function sets the NASS API key, fetches agricultural land data related to irrigated acres and the number of operations
#' for a specified year and state, cleans, and merges these datasets. The function ensures that there
#' are no discrepancies between the datasets before merging. If discrepancies are found, it returns an empty
#' data frame with a warning message. Otherwise, it returns a cleaned and merged dataset.
#'
#' @param Year A numeric value representing the year for which the data is to be fetched.
#' @param state_name A character string specifying the name of the state for which the data is to be fetched.
#' @param my_nass_key A character string representing the NASS API key required for accessing the data.
#' @return A data frame containing the merged and cleaned dataset of irrigated acres and number of operations
#'         for the specified year and state. If discrepancies are found, an empty data frame is returned with a warning.
#' @examples
#' # Assuming nass_data, clear_data_acres, and clear_data_ops functions are defined and available,
#' # and you have a valid NASS API key:
#' my_api_key <- "your_nass_api_key_here"
#' nass_data_2022_SC <- get_nass_data(2022, "South Carolina", my_api_key)
#' @export
#'
#' @importFrom dplyr select filter
#' @importFrom stringr str_detect
get_nass_data <- function(Year, state_name, my_nass_key) {

  # Set the NASS API key
  nass_set_key(my_nass_key)

  # Convert state name to uppercase
  states.keep <- toupper(state_name)

  # Fetch and clean acres data
  nass.ag.data <- nass_data(year = Year,
                            commodity_desc = "AG LAND",
                            short_desc = "AG LAND, IRRIGATED - ACRES",
                            domain_desc = c("AREA OPERATED", "TOTAL"),
                            agg_level_desc = "COUNTY",
                            state_name = states.keep)
  nass.ag.data <- clear_data_acres(nass.ag.data)

  # Fetch and clean operations data
  nass.ops.data <- nass_data(year = Year,
                             commodity_desc = "AG LAND",
                             short_desc = "AG LAND, IRRIGATED - NUMBER OF OPERATIONS",
                             domain_desc = c("AREA OPERATED", "TOTAL"),
                             agg_level_desc = "COUNTY",
                             state_name = states.keep)
  nass.ops.data <- clear_data_ops(nass.ops.data)

  # Check for discrepancies
  check <- subset(nass.ag.data, !(fips %in% nass.ops.data$fips))

  # Only proceed if 'check' is empty
  if (nrow(check) == 0) {
    # Merge the datasets if no discrepancies are found
    nass_binned <- merge(nass.ag.data[,c(1:8)], nass.ops.data[,c(1,6:8)], by = c("fips", "Size.bins", "Year"))

    # Handle missing values
    nass_binned$Irr.Area.Acres[is.na(nass_binned$Irr.Area.Acres)] <- "-9999"

    return(nass_binned)
  } else {
    # Return a message or an empty data frame if discrepancies exist
    warning("Discrepancies found. nass_binned not created.")
    return(data.frame()) # or you might want to handle this situation differently
  }
}
