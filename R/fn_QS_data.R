#' Query and Summarize NASS Data for a Specific Year
#'
#' This function processes NASS data to produce summaries for a given year. It generates three separate dataframes:
#' one for county summaries, one for binned operations, and one for binned irrigated areas. It filters data for the specified year,
#' counts the number of counties with irrigation data by farm size, and constructs summary tables for operations and acreage by size bins.
#'
#' @param YEAR A numeric value indicating the year for which the data should be summarized.
#' @return A list containing three dataframes: `binned_operations`, `binned_irrigated_area`, and `County_Summary`.
#' @examples
#' # Assuming nass_binned is available in your environment:
#' result <- QS_data(2022)
#' binned_operations <- result$binned_operations
#' binned_irrigated_area <- result$binned_irrigated_area
#' County_Summary <- result$County_Summary
#' @export
#'
#' @importFrom dplyr filter select
#' @importFrom stats setNames
#'
#'
QS_data <- function(YEAR) {

  #Total gives county Totals for irrigated area and Irrigated operations

  County_summary <- subset(nass_binned, Domain == "TOTAL")

  # County Totals for a particular year

  dat_YEAR_QS <-   filter(County_summary, Year == YEAR )

  # This gives irrigated acreage and operations for each size bin in a county for a particular year
  QS_year <- filter(nass_binned, Year == YEAR )

  ## Count number of counties with irrigation data by farm size
  size.id <- grep(pattern="AREA OPERATED", QS_year$Size.bins)
  farm.size <- QS_year[size.id ,]              # Remove the total values

  n.counties <- length(unique(farm.size$fips))
  n.sizebins <- length(unique(farm.size$Size.bins))

  farm.size.ops <- matrix(data=NA, ncol=n.sizebins, nrow=n.counties)
  colnames(farm.size.ops) <- unique(farm.size$Size.bins)
  rownames(farm.size.ops) <- unique(farm.size$County_Name)

  farm.size.size <- farm.size.ops

  for (c in 1:n.counties){
    county.data <- subset(farm.size, fips == unique(farm.size$fips)[c])
    county.ops <- county.data[, -c(7,8)]
    county.sizes <- county.data[, -c(7,9)]
    for (s in 1:n.sizebins){
      size.ops<-subset(county.ops, Size.bins == unique(farm.size$Size.bins)[s])
      size.sizes<-subset(county.sizes, Size.bins == unique(farm.size$Size.bins)[s])
      if (dim(size.ops)[1] == 1) {farm.size.ops[c,s] <- paste(size.ops$Irr.Ops)}
      if (dim(size.sizes)[1] == 1) {farm.size.size[c,s] <- paste(size.sizes$Irr.Area.Acres)}
    }
  }
  rm(county.ops, county.sizes, county.data)


  # Summary Table
  data.summary <- data.frame(County_Name = unique(farm.size$County_Name),
                             fips = rep(NA,n.counties),
                             Irrigated_Operations = rep(NA, n.counties),
                             Irrigated_Acreage = rep(NA, n.counties),
                             Size_Bins = rep(NA, n.counties),
                             ops.bins = rep(NA, n.counties)
  )
  data.summary$County_Name <- as.character(paste(data.summary$County_Name))


  for (c in 1:n.counties){
    County_Name<-data.summary$County_Name[c]
    data.summary$fips[c] <- as.character(paste(dat_YEAR_QS$fips[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]))
    data.summary$Irrigated_Operations[c] <- dat_YEAR_QS$Irr.Ops[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]
    data.summary$Irrigated_Acreage[c] <- dat_YEAR_QS$Irr.Area.Acres[
      as.character(paste(dat_YEAR_QS$County_Name)) == County_Name]

    # Binned data
    county.ops<-farm.size.ops[rownames(farm.size.ops) == County_Name,]
    data.summary$ops.bins[c] <-  sum(!is.na(county.ops))
    county.sizes<-farm.size.size[rownames(farm.size.size) == County_Name,]
    data.summary$Size_Bins[c]<- sum(!is.na(county.sizes) & county.sizes != -9999)
  }

  binned_operations <<- as.data.frame(farm.size.ops[, c("AREA OPERATED: (1.0 TO 9.9 ACRES)" ,    "AREA OPERATED: (10.0 TO 49.9 ACRES)",  "AREA OPERATED: (50.0 TO 69.9 ACRES)" ,
                                                          "AREA OPERATED: (70.0 TO 99.9 ACRES)","AREA OPERATED: (100 TO 139 ACRES)" ,"AREA OPERATED: (140 TO 179 ACRES)"   ,
                                                        "AREA OPERATED: (180 TO 219 ACRES)"   ,"AREA OPERATED: (220 TO 259 ACRES)", "AREA OPERATED: (260 TO 499 ACRES)"     ,
                                                        "AREA OPERATED: (500 TO 999 ACRES)"  ,  "AREA OPERATED: (1,000 TO 1,999 ACRES)",   "AREA OPERATED: (2,000 OR MORE ACRES)"  )])
  # No of operations for each size bin in a county
  binned_irrigated_area  <<- as.data.frame(farm.size.size[, c("AREA OPERATED: (1.0 TO 9.9 ACRES)" ,    "AREA OPERATED: (10.0 TO 49.9 ACRES)",  "AREA OPERATED: (50.0 TO 69.9 ACRES)" ,
                                                              "AREA OPERATED: (70.0 TO 99.9 ACRES)","AREA OPERATED: (100 TO 139 ACRES)" ,"AREA OPERATED: (140 TO 179 ACRES)"   ,
                                                              "AREA OPERATED: (180 TO 219 ACRES)"   ,"AREA OPERATED: (220 TO 259 ACRES)", "AREA OPERATED: (260 TO 499 ACRES)"     ,
                                                              "AREA OPERATED: (500 TO 999 ACRES)"  ,  "AREA OPERATED: (1,000 TO 1,999 ACRES)",   "AREA OPERATED: (2,000 OR MORE ACRES)"  )]) # Irrigated Acreage for each size bin in a county
  County_Summary <<- data.summary





####Bin dat summary ###########################################################
# bin.char is a dataframe for the summaries of binned irrigated area.
# bin.table$irr.AcresperOp: This calculates the number of irrigated acres per operation for each county
# that has both operations and acreage information. It’s calculated by dividing the number of irrigated
# acres by the number of operations.
# bin.table$irr.perc: This calculates the percentage of the farm that is irrigated. It’s calculated by
# dividing irr.AcresperOp by the average size of the farms in the bin (size.avg).
# bin.char$avg_perc_irr[i]: This calculates the average percentage of farm irrigation for the bin.
# It’s calculated by taking the mean of irr.perc for all the counties in the bin, ignoring NA values.



data.ops <- binned_operations
data.ops <-cbind(rownames(data.ops),data.ops)
data.size <- binned_irrigated_area
data.size <-cbind(rownames(data.size),data.size)

# Create bin.char to summarize information for each size bin
n.sizes <-ncol(data.ops) - 1
bin.char<-data.frame(size=colnames(data.ops)[2:(n.sizes+1)],
                     minsize = rep(NA,n.sizes), maxsize = rep(NA,n.sizes),
                     avgsize = rep(NA,n.sizes))
bin.char$minsize <- c(1, 10, 50, 70, 100, 140, 180, 220, 260, 500, 1000, 2000)
bin.char$maxsize <- c(9.9, 49.9, 69.9, 99.9, 139, 179, 219, 259, 499, 999, 1999, 5000)
bin.char$avgsize <- apply(bin.char[,2:3],1,"mean")
bin.char$size<-as.character(bin.char$size)
bin.char$cntys_w_data <- NA
bin.char$cntys_w_Ds <- NA
bin.char$avg_perc_irr <- NA

# Create an object that lists size groupings in a nice format for plotting
size.bins <-gsub(pattern="AREA.OPERATED...",replacement="",x=bin.char$size)
size.bins <-gsub(pattern=".TO.", replacement=" to ", x=size.bins)
size.bins <-gsub(pattern=".ACRES.", replacement=" acres", x=size.bins)
size.bins <-gsub(pattern=".OR.MORE.", replacement=" or more ", x=size.bins)



# Create a table for each bin size to store all county info for farms of that size
# Store in a list of length 12
bin.table <- data.frame(County=as.character(data.ops[,1]),
                        irr.ops=rep(NA,dim(data.ops)[1]),
                        irr.acres=rep(NA,dim(data.ops)[1]),
                        irr.AcresperOp=rep(NA,dim(data.ops)[1]),
                        irr.perc=rep(NA,dim(data.ops)[1]))
bin.list <- rep(list(bin.table),length(size.bins))

for (i in 1:length(size.bins)){
  bin.table <- bin.list[[i]]
  size <- bin.char$size[i]
  bin.table$irr.ops <- data.ops[,colnames(data.ops) == size]
  bin.table$irr.acres <- data.size[,colnames(data.size) == size]

  # remove any commas in irr.acres
  bin.table$irr.acres <- gsub(pattern=",",replacement="",x=bin.table$irr.acres)

  # How many counties have operations and acreage info
  has.ops <- !is.na(bin.table$irr.ops)
  has.acres <- !is.na(bin.table$irr.ops) & bin.table$irr.acres != "-9999"
  bin.table$irr.AcresperOp[has.acres] <- as.numeric(paste(bin.table$irr.acres[has.acres]))/
    as.numeric(paste(bin.table$irr.ops[has.acres]))
  size.avg <-bin.char$avgsize[i]
  bin.table$irr.perc <- bin.table$irr.AcresperOp/size.avg

  # Save info to bin.char
  bin.char$cntys_w_data[i] <- sum(has.acres)
  bin.char$avg_perc_irr[i] <- mean(bin.table$irr.perc,na.rm=TRUE)
  bin.char$cntys_w_Ds[i] <- sum(has.ops) - sum(has.acres)

  # create and save figure

  # ppi<-300
  # filename<-paste0(WUDR_github,"/plots/Census_data_bin.char/",YEAR,"_",size.bins[i],".png")
  # png(file=filename,width=4*ppi,height=4*ppi,res=ppi)
  # hist(bin.table$irr.perc, main=size.bins[i], xlab="Percent of Farm Irrigated")
  # abline(v=mean(bin.table$irr.perc,na.rm=TRUE), col="red",lwd=2)
  # dev.off()

  # Save size category table to list
  bin.list[[i]] <- bin.table

  bin.char <<- bin.char
}

##############################################################################################
##############################################################################################
##############################################################################################
# Assuming 'binned_irrigated_area' is a dataframe with counties as row names
# binned_irrigated_area is the data for Irrigated operations at "Bin" Level.
# This along with the data at county level is used to calculted Irrigated area in "D" values in
# summary.irr

# Convert binned_irrigated_area to numeric, handling NAs
Irrigated.data <- as.data.frame(lapply(binned_irrigated_area, function(x) {
  nums <- as.numeric(as.character(x))
  ifelse(is.na(nums), 0, nums) # Replace NAs with 0
}))

# Add county names as the first column
Irrigated.data$County <- rownames(binned_irrigated_area)

# Rearrange columns to have 'County' as the first column
Irrigated.data <- Irrigated.data[, c(ncol(Irrigated.data), 1:(ncol(Irrigated.data)-1))]

# Update column names to include 'County' and size bins
colnames(Irrigated.data) <- c("County", "below10", "between_10_50", "between_50_70", "between_70_100",
                              "between_100_139", "between_140_179", "between_180_219", "between_220_259",
                              "between_260_499", "between_500_999", "between_1000_1999", "Above_2000")

summary.irr <- data.frame(County=as.character(Irrigated.data$County),
                          Irri.sum=rep(NA,dim(Irrigated.data)[1]))

sum_dat <- Irrigated.data[,-1]
sum_dat[sum_dat=="-9999"]<-0

summary.irr$Irri.sum <- rowSums(sum_dat, na.rm = TRUE)


###Load County Summary data
census.data <- County_Summary

summary.irr$County.sum <- as.numeric(census.data$Irrigated_Acreage)

summary.irr$County.sum <- ifelse(summary.irr$County.sum=="-9999", summary.irr$Irri.sum, summary.irr$County.sum)

summary.irr$D_area <- summary.irr$County.sum - summary.irr$Irri.sum

##############################################################################################
##############################################################################################
##############################################################################################
# Let's start with filling the D values.

Irrigated.data <- as.data.frame(lapply(binned_irrigated_area, function(x) {
  nums <- as.numeric(as.character(x))
  ifelse(is.na(nums), 0, nums) # Replace NAs with 0
}))

# Add county names as the first column
Irrigated.data$County <- rownames(binned_irrigated_area)

# Rearrange columns to have 'County' as the first column
Irrigated.data <- Irrigated.data[, c(ncol(Irrigated.data), 1:(ncol(Irrigated.data)-1))]

# Update column names to include 'County' and size bins
colnames(Irrigated.data) <- c("County", "below10", "between_10_50", "between_50_70", "between_70_100",
                              "between_100_139", "between_140_179", "between_180_219", "between_220_259",
                              "between_260_499", "between_500_999", "between_1000_1999", "Above_2000")

# Transform Irrigated.data into a list of data frames, one per county
area_list <- lapply(split(Irrigated.data, Irrigated.data$County), function(df) {
  t_df <- t(df[,-1])  # Exclude 'County' column and transpose
  colnames(t_df) <- df$County[1]  # Use the first county name as the column name for transposed data
  return(as.data.frame(t_df))
})


# Filter the lists which the size bins containing D values
sorted <- list()
for (i in 1:length(area_list)) {
  x <- as.data.frame(area_list[[i]])  # Convert each element to a data frame
  x$size.bins <- rownames(x)  # Ensure 'size.bins' column is correctly assigned from rownames

  # Filter for rows where the first column (assumed to be the values column) equals -9999
  x <- filter(x, x[,1] == -9999)

  # Assign the filtered data frame to the 'sorted' list
  sorted[[i]] <- x
}

# Assign names to the 'sorted' list to match those of 'area_list'
names(sorted) <- names(area_list)

size.names <- c( "below10", "between_10_50","between_50_70","between_70_100"
                   ,"between_100_139","between_140_179","between_180_219","between_220_259","between_260_499"
                   ,"between_500_999","between_1000_1999", "Above_2000")

bin.char$size <- size.names

# 'sorted' now contains data frames for counties, filtered for rows where any size bin had '-9999'

# Replace the process for Opearations as well

# Convert operations data to numeric, handling NAs
# Convert operations data to numeric, replacing NAs with -99
  Operations.data <- binned_operations
  Operations.data[] <- lapply(Operations.data, function(x) {
    as.numeric(as.character(x))
  })
  Operations.data[is.na(Operations.data)] <- -99

  # Add county names as the first column
  county_names <- rownames(binned_operations)
  Operations.data <- cbind(County = county_names, Operations.data)

  # Define column names including "County"
  Colnames <- c("County", "below10", "between_10_50", "between_50_70", "between_70_100",
                "between_100_139", "between_140_179", "between_180_219", "between_220_259",
                "between_260_499", "between_500_999", "between_1000_1999", "Above_2000")
  colnames(Operations.data) <- Colnames


  # Ensure the row names are set correctly to the county names
  rownames(Operations.data) <- Operations.data[, 1]
  Operations.data <- Operations.data[, -1]

  # Splitting and transposing Operations.data for each county
  operations_list <- setNames(split(Operations.data, rownames(Operations.data)), rownames(Operations.data))

  # Transposing each county's data frame within the list
  for (i in seq_along(operations_list)) {
    operations_list[[i]] <- as.data.frame(t(operations_list[[i]]))
    colnames(operations_list[[i]]) <- c("Ops")
    operations_list[[i]]$ops.bins <- rownames(operations_list[[i]])
    rownames(operations_list[[i]]) <- NULL
  }

  operations_list <<- operations_list
############################################################################
  # Create a new list called percetage list where we merge the sorted listed with bin.char to
  #  get the average irrigated area in each size


  pct.irr <- list()

  for (i in 1:length(sorted)) {
    # Assuming sorted[[i]] has 'size.bins' after previous corrections
    x <- as.data.frame(sorted[[i]])


    x <- merge(bin.char[, c(1, 4, 7)], x, by.x = "size", by.y = "size.bins")
    x <- as.data.frame(x[,-c(4)])

    if (i <= length(operations_list)) {
      y <- as.data.frame(operations_list[[i]])

      # Merge 'x' and 'y' based on the 'size' column, which is present in both
      # This assumes 'y' (operations data) is prepared with 'size' (or similar) and 'Ops' columns
      z <- merge(x, y, by.x = "size", by.y = "ops.bins")

      # Store the merged result
      pct.irr[[i]] <- z
    }
  }

  # Set column names for elements in pct.irr list as per your specification
  for (i in seq_along(pct.irr)) {
    colnames(pct.irr[[i]]) <- c("Size", "Avg_Size", "Avg_Pct_Irri", "Ops")
  }

###############################################################################
  # Now we grab the D value area from summary.irr

  area.irr <- list()
  for (i in 1:length(pct.irr)) {

    x <- as.data.frame(pct.irr[[i]])
    x$area <- x$Avg_Pct_Irri * x$Avg_Size * x$Ops

    area.irr[[i]] <- x
  }  # Irrigated area for D values


  recaled_dat <- area.irr

  for (i in 1:length(area.irr)) {

    recaled_dat[[i]][6] <- Map(function(x, y)  x * y/sum(x), area.irr[[i]][5], as.numeric(summary.irr[i,4]))

  }

  sum(recaled_dat[[1]][6])

  # THis rescaled data list replaces the D values after rescaling it to difference

  colnames <- c("Size bin", "Avg_Size", "Average Pct Irri", "Ops", "Area Irrigated", " Rescaled Area")

  recaled_dat <- lapply(recaled_dat, setNames, colnames)

  names(recaled_dat) <- names(area_list)


dat_D_filled <- list()
for (i in 1:length(recaled_dat)) {
  x <- as.data.frame(recaled_dat[[i]])
  y <- as.data.frame(area_list[[i]])
  y$size <- rownames(y)
  z <-  full_join(x, y , by = c("Size bin"= "size"))
  z[,7] <- ifelse((z[,7] == -9999), z[,6], z[,7])
  z <- as.data.frame(z[,c(1,7)])
  z[,2] <- round(z[,2], 0)
  dat_D_filled[[i]] <- z
}



colnames <- c("Size bin",  "Area_irr_rescaled Area")


names(dat_D_filled) <- names(area_list)

dat_D_filled <<-dat_D_filled

}

