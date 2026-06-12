library(rlpi)
library(missMethods)
library(tidyverse)

route <- '03processedData/'
route2 <- '04FinalData/'

set.seed(42)  # For reproducibility
source('01Scripts/Functions.r')

# Example simulation
sim_result <- pop_growth(N0 = 10, gen = 500, stochastic_r = TRUE, stochastic_K = TRUE, plotting = T)
# Add observation error to simulation results
obs_error <- rbinom( n = length(sim_result),  size = round(sim_result), p = 0.1)
plot((sim_result +obs_error), col = 'red',  pch = 20)

# Simulate multiple species
num_years <- 500
num_species <- 40000
#species_data <- data.frame(matrix(NA, nrow = num_species, ncol = num_years))
species_data <- matrix(NA, nrow = num_species, ncol = num_years)
ran_pop_sizes <- numeric(num_species)
#ran_pop_sizes <- NA
start_time <- Sys.time()

for (i in 1:num_species) {
    init_pop <- as.integer(10^(runif(1, min = 1.1, max = 5)))
    
    sim_result <- pop_growth(N0 = init_pop, gen = num_years - 1, stochastic_r = TRUE, stochastic_K = TRUE, plotting = FALSE)
    
    # Add observation error
    obs_error <- rbinom( n = length(sim_result),  size = round(sim_result), p = 0.1)
    sim_result <- sim_result + obs_error
    species_data[i,] <- sim_result
    ran_pop_sizes[i] <- init_pop
    
    #ran_pop_sizes <- c(ran_pop_sizes, init_pop)
} ;end_time <- Sys.time()

end_time - start_time #37.68823 mins

# Remove time series with zero values after 200 generations
species_data2 <- as.data.frame((species_data))
species_data_clean <- species_data2 %>%
  filter(complete.cases(.[, 200:ncol(.)])) %>%
  filter(rowSums(.[, 200:ncol(.)] == 0) == 0) %>%
  filter(rowSums(.[, 200:ncol(.)] != 0) == (ncol(.) - 199))
any(species_data_clean == 0)
# Subset the cleaned data for a specific range

# Save cleaned data to disk
dir.create(paste0(route))
dir.create(paste0(route2))

save(species_data_clean, file = paste0(route,'Species_simulations.RData'))

# Load and plot subset data
#load(file = paste0('03processedData/Species_simulations.RData'))
plot(as.numeric(species_data_clean[sample(1:nrow(species_data_clean),1),]))

# Simulate data matrix for LPI structure
#To process it you should download the LPD data from the LPI website https://www.livingplanetindex.org/data_portal and save it in the folder 00RawData.
# The file name should be adjusted if it is different
lpi_data <- read.csv('00RawData/LPD_2024_public.csv') #include in this folder the LPD data
lpi_data = filter(lpi_data, Replicate == 0) # Remove replicates

years <- as.numeric(gsub("X", "",(names(lpi_data)[grepl(paste0("^", "X", "[0-9]+$"),  names(lpi_data))]))) ## Modified to add the same number of years in the LPI
S <- nrow(lpi_data) ## Modified to add the same number of rows in the LPI

# Select the last population generation
cx <- ncol(species_data_clean) - length(years) + 1
species_data_subset <- species_data_clean[, cx:(cx + length(years) - 1)]

# Select randomly the number of population generation randoml
sp_data_sub2 <- species_data_subset <- species_data_subset[sample(nrow(species_data_subset), S),]

plot(as.numeric(species_data_subset[sample(1:nrow(species_data_subset),1),]))
species_data_subset<- bino_id(species_data_subset, years, S)

dir.create("03processedData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

# Compute LPI using the complete simulated population dataset 
lpi_result <- LPIMain(
  create_infile(species_data_subset, index_vector = TRUE, name = '03processedData/complete/simulated/Complete_DataSet', 
                start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Simulated Data - Full Dataset', REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_result <- lpi_result[-nrow(lpi_result), ] #removing the “2021” value that is just a duplicate placeholder
lpi_result$years <- years

dir.create("04FinalData/complete/simulated/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

names(lpi_result)
# assume your data frame is called df and X is the column you want to rename
names(lpi_result) <- ifelse(names(lpi_result) == "X", "years", names(lpi_result))

write.csv(lpi_result, '04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')
lpi_result <- read.csv('04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')

# Read and process real LPI data
###################################

# Compute the LPI using the empirical population data present in the LPD 
dir.create("03processedData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

## unweighted population level LPI withreplicated excluded
lpi_data_poplevel <- lpi_data
lpi_data_poplevel$Binomial = paste0(lpi_data_poplevel$Binomial, "_", lpi_data_poplevel$ID)


lpi_resultR <- LPIMain(
  create_infile(lpi_data_poplevel, index_vector = TRUE, name = '03processedData/complete/real/Complete_dataSet', 
                start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
  title = 'LPI Results Real Data', REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
)

lpi_data_filtered <- lpi_data %>% select(matches("^X[0-9]"))
lpi_data_filtered <- clean_data(lpi_data_filtered)
TotofZeros <- colSums(lpi_data_filtered == 0, na.rm = T)
lpi_resultR <- lpi_resultR[-nrow(lpi_resultR),]
lpi_resultR$years <- years
lpi_resultR$numZeros <- TotofZeros

dir.create("04FinalData/complete/real/Complete_dataSet",
           recursive = TRUE,
           showWarnings = FALSE)

# assume your data frame is called df and X is the column you want to rename
names(lpi_resultR) <- ifelse(names(lpi_resultR) == "X", "years", names(lpi_resultR))

write.csv(lpi_resultR, '04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')
lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')

###########
### END ###
###########