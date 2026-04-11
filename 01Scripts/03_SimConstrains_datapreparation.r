library(tidyverse)
library(rlpi)
library(data.table)

#functions 
source('01Scripts/Functions.r')

# Define all the unique path components
base_paths <- c(
  "03processedData/constrain/3_zero_permutations",
  "03processedData/constrain/2_na_permutations", 
  "03processedData/constrain/1_na_zero_permutations",
  "04FinalData/constrain/1_na_zero_permutations",
  "04FinalData/constrain/2_na_permutations",
  "04FinalData/constrain/3_zero_permutations"
)

sub_paths <- c(
  "without_permutation",
  "simulatedData/processing"
)

# Create all directories
for (base in base_paths) {
  for (sub in sub_paths) {
    dir.create(file.path(base, sub), recursive = TRUE, showWarnings = FALSE)
  }
}
# Set seed for reproducibility
set.seed(42)

# LPD
lpi_data <- read.csv('00RawData/LPD_2024_public.csv')
lpi_data <- dplyr::filter(lpi_data, Replicate == 0) # Remove replicates
lpi_data_filtered <- lpi_data %>% select(matches("^X[0-9]"))
lpi_data_filtered <- clean_data(lpi_data_filtered)
mask <- is.na(lpi_data_filtered) | lpi_data_filtered == 0
print("Mising and zero values")
mask[is.na(mask)] <- FALSE
table(mask)
maskNA <- is.na(lpi_data_filtered)
print("Mising values")
table(maskNA)
mask0 <- lpi_data_filtered == 0
mask0[is.na(mask0)] <- FALSE
table(mask0)
print("Zero values")

years <- as.numeric(gsub("X", "",(names(lpi_data)[grepl(paste0("^", "X", "[0-9]+$"),  names(lpi_data))]))) ## Modified to add the same number of years in the LPI
S <- nrow(lpi_data) ## Modified to add the same number of rows in the LPI

## simulated data matrix
load(file = '03processedData/Species_simulations.RData')
# Simulate data matrix for LPI structure
# Select randomly the population generation
cx <- ncol(species_data_clean) - length(years) + 1
species_data_subset <- species_data_clean[, cx:(cx + length(years) - 1)]
species_data_subset <- species_data_subset[sample(nrow(species_data_subset), nrow(lpi_data_filtered)),]

#Real LPI data
lpi_data_filteredNA <- lpi_data_filteredZero <- lpi_data_filteredZeroNA <- lpi_data_filtered

####################
# 1. NAs and Zero ##
####################
lpi_data_filteredZeroNA[!mask] <- species_data_subset[!mask]
# Fill simulated data matrix with NA's in the same position as the real dataset
print("are the NA's values at the same position?")
identical(is.na(lpi_data_filtered), is.na(lpi_data_filteredZeroNA))
print("are the Zeros's values at the same position?")
identical((lpi_data_filtered == 0), (lpi_data_filteredZeroNA == 0))

lpi_data_filteredZeroNA <- bino_id(lpi_data_filteredZeroNA, years, S)

# Compute LPI using simulated data and adding NA based in the real dataset
lpi_resultNAZero <- LPIMain(
  create_infile(lpi_data_filteredZeroNA, index_vector = TRUE, name = '03processedData/constrain/1_na_zero_permutations/without_permutation/without_permutationNAand0', 
                start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - NA real dataaset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_resultNAZero$years <- years # Add the year
TotofZeros <- colSums(lpi_data_filtered == 0, na.rm = T) # Count the number of zeros per year
lpi_resultNAZero$numZeros <- TotofZeros # Add, per year, the number of zeros 

write.csv(lpi_resultNAZero, '04FinalData/constrain/1_na_zero_permutations/without_permutation/without_permutationNAand0.csv')
#lpi_resultNAZero <- read.csv('04FinalData/constrain/without/without_permutationNAand0.csv')

########################
# 2. NAs permutations ##
########################
lpi_data_filteredNA[!maskNA] <- species_data_subset[!maskNA]

# Fill simulated data matrix with NA's in the same position as the real dataset
print("are the NA's matrices similar?")
identical(is.na(lpi_data_filtered), is.na(lpi_data_filteredNA))
lpi_data_filteredNA_unique <- bino_id(lpi_data_filteredNA, years, S)

# Compute LPI using simulated data and adding NA based in the real dataset
lpi_resultNA <- LPIMain(
  create_infile(lpi_data_filteredNA_unique, index_vector = TRUE, name = '03processedData/constrain/2_na_permutations/without_permutation/without_permutationNA', 
                start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - NA real dataaset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

dir.create("04FinalData/constrain/2_na_permutations/without_permutation/",
           recursive = TRUE,
           showWarnings = FALSE)

lpi_resultNA$years <- years
write.csv(lpi_resultNA, '04FinalData/constrain/2_na_permutations/without_permutation/without_permutationNA.csv')
#lpi_resultNA <- read.csv('04FinalData/constrain/without_permutation/without_permutationNA.csv')

###########################
## 3. Zeros permutations ##
###########################
lpi_data_filteredZero[!mask0] <- species_data_subset[!mask0]

#### Zero position are identical in both matrices
print("are the zero matrices similar?")
identical((ifelse(is.na(lpi_data_filteredZero), FALSE, lpi_data_filteredZero == 0)),
(ifelse(is.na(lpi_data_filtered), FALSE, lpi_data_filtered == 0)))

lpi_data_filtered0_unique <- bino_id(lpi_data_filteredZero, years, S)

# Compute LPI using simulated data and adding NA based in the real dataset
lpi_resultzero <- LPIMain(
  create_infile(lpi_data_filtered0_unique, index_vector = TRUE, name = '03processedData/constrain/3_zero_permutations/without_permutation/without_permutationzero', 
                start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - NA real dataaset', REF_YEAR = 1950, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_resultzero$years <- years
  write.csv(lpi_resultzero, '04FinalData/constrain/3_zero_permutations/without_permutation/without_permutationzero.csv')
#lpi_resultzero <- read.csv('04FinalData/constrain/3_zero_permutations/without_permutation/without_permutationzero.csv')

#######################################################################################################
##### Data preparation for geenrate 300 permutations per approach to test their effect in the result ##
#######################################################################################################
lpi_data_filteredNA <- lpi_data_filteredZero <- lpi_data_filteredZeroNA <- lpi_data_filtered
lpi_data_filteredNA[!mask] <- species_data_subset[!mask]
lpi_data_filteredZeroNA[!mask] <- species_data_subset[!mask]

lpi_data_filteredNAZeroPer <- permutationLPI(lpi_data_filteredZeroNA, nperm = 300, shuffle_zeros = TRUE, shuffle_NA = TRUE , years, S, summary = T)
lpi_data_filteredNAPer <- permutationLPI(lpi_data_filteredNA, nperm = 300, shuffle_zeros = FALSE, shuffle_NA = TRUE , years, S, summary = T)
lpi_data_filteredZeroPer <- permutationLPI(lpi_data_filteredZero, nperm = 300, shuffle_zeros = TRUE, shuffle_NA = FALSE , years, S, summary = T)

## Simulated Data 
for (i in 1:length(lpi_data_filteredNAZeroPer)) {
  subdir <- sprintf("03processedData/constrain/1_na_zero_permutations/simulatedData/processing/")
  saveRDS(
    lpi_data_filteredNAZeroPer[[i]],
    file = file.path(subdir, sprintf("matrix_%03d.rds", i))
  )
}

for (i in 1:length(lpi_data_filteredNAPer)) {
  subdir <- sprintf("03processedData/constrain/2_na_permutations/simulatedData/processing/")
  saveRDS(
    lpi_data_filteredNAPer[[i]],
    file = file.path(subdir, sprintf("matrix_%03d.rds", i))
  )
}

for (i in 1:length(lpi_data_filteredZeroPer)) {
  subdir <- sprintf("03processedData/constrain/3_zero_permutations/simulatedData/processing/")
  saveRDS(
    lpi_data_filteredZeroPer[[i]],
    file = file.path(subdir, sprintf("matrix_%03d.rds", i))
  )
}
##############
#### END #####
##############  