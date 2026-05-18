library(rlpi)
library(missMethods)
library(tidyverse)
library(data.table)

source('01Scripts/Functions.r')

# For reproducibility
set.seed(42)

# Trend simulation and comparison
#To process it you should download the LPD data from the LPI website https://www.livingplanetindex.org/data_portal and save it in the folder 00RawData.
# The file name should be adjusted if it is different
lpi_data <- read.csv('00RawData/LPD_2024_public.csv') #include in this folder the LPD data
lpi_data <- dplyr::filter(lpi_data, Replicate == 0) # Remove replicates
years <- as.numeric(gsub("X", "",(names(lpi_data)[grepl(paste0("^", "X", "[0-9]+$"),  names(lpi_data))]))) ## Modified to add the same number of years in the LPI
S <- nrow(lpi_data) ## Modified to add the same number of rows in the LPI

x <- seq(0, 1, length.out = length(years))
vect_conv <- round(((60 * (1 - x^0.2)) + 40), 2); plot(vect_conv)
vect_linD <- round(((60 * (1 - x^1)) + 40), 2);  plot(vect_linD)
vect_conc <- round(((60 * (1 - x^5)) + 40), 2); plot(vect_conc)

# Compare different trend matrices
trend_list <- list(vect_conv, vect_linD, vect_conc)
trend_matrices <- list()

# Load and plot subset data
load(file = '03processedData/Species_simulations.RData')

# Select randomly the population generation
cx <- ncol(species_data_clean) - length(years) + 1
species_data_subset <- species_data_clean[, cx:(cx + length(years) - 1)]

# Select randomly the number of population generation
species_data_subset <- species_data_subset[sample(nrow(species_data_subset), S),]
plot(as.numeric(species_data_subset[sample(1:nrow(species_data_subset),1),]))

for (i in 1:3) {
  trend_matrices[[i]] <- sweep(species_data_subset, 2, trend_list[[i]] / 100, "*")
} 

# Verify similarity between trend matrices - It should be different 
print("are identical the matrix 1 and 2?")
identical(trend_matrices[[1]], trend_matrices[[2]])
print("are identical the matrix 1 and 3?")
identical(trend_matrices[[1]], trend_matrices[[3]])
print("are identical the matrix 2 and 3?")
identical(trend_matrices[[2]], trend_matrices[[3]])

plot(as.numeric(as.matrix(trend_matrices[[1]][1:100, ])))
plot(as.numeric(as.matrix(trend_matrices[[2]][1:100, ])))
plot(as.numeric(as.matrix(trend_matrices[[3]][1:100, ])))

# Add ID, binomial and ID
lpi_trend_matrices <- lapply(
  trend_matrices,
  bino_id,
  years =years,
  num_species = S
)

# Simulate trends for different datasets
trend_names <- c('Convex Decrease', 'Linear Decrease', 'Concave Decrease')
high_results <- list()

dir.create("03processedData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)

# Loop to generate LPI for each trend matrix
for (i in 1:length(lpi_trend_matrices)) {
  high_results[[i]] <- LPIMain(
    create_infile(lpi_trend_matrices[[i]], index_vector = TRUE, 
                  name = paste0('03processedData/complete/simulated/Conv_conc_lin/', trend_names[i]),
                  start_col_name = "X1950", end_col_name = "X2020", CUT_OFF_YEAR = 1950),
    title = trend_names[i], REF_YEAR = 1950, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE = FALSE
  )
}

# Unify plots
labels <- c("concave", "linear", "convex")
 colors <- c("#558ed5", "#77933d", "#4a452a")  # your specified colors

to_plot<- out <- bind_rows(
  lapply(seq_along(high_results), function(i) {
    df <- as.data.frame(high_results[[i]])
    df <- lpi_result[-nrow(lpi_result), ] # removing the “2021” value that is just a duplicate placeholder
    df$years <- 1950:2020
    df$label <- labels[i]
    df$sim <- 1
    df
  })
)

to_plot <- to_plot[to_plot$years != 2021, ]# removing the “2021” value that is just a duplicate placeholder
dir.create("04FinalData/complete/simulated/Conv_conc_lin/",
           recursive = TRUE,
           showWarnings = FALSE)


write.csv(to_plot, '04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')
#to_plot <- read.csv('04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')

#####################################################################################################################
## Loop to computes the LPI with linear, concave and convex removing 0, 20%, 40%, 60% y 80% percent of the dataset
#####################################################################################################################

dir.create("03processedData/complete/simulated/Conv_conc_lin_Remdt/",
           recursive = TRUE,
           showWarnings = FALSE)

### Remove randomlty data
red_values <- seq(.2, .8, by =.2)
red_values <- c(red_values, 0.95)
remove_data_vect <- list()
ad<- list()

## Loop to remove data using the original
remove_data_vect <- vector("list", length(lpi_trend_matrices))
n_rep <-50

for (g in seq_along(lpi_trend_matrices)) {
  trend_list <- vector("list", length(red_values))
  
  for (w in seq_along(red_values)) {
    
    rep_list <- vector("list", n_rep)
    for (r in seq_len(n_rep)) {
      tmp <- delete_MCAR(
        lpi_trend_matrices[[g]][, 1:71],
        red_values[w]
      )
      rep_list[[r]] <- bino_id(tmp, years, S)
    }
    trend_list[[w]] <- rep_list
  } 
  names(trend_list) <- paste0("remove_", red_values)
  remove_data_vect[[g]] <- trend_list
}
#Check the names fo the df
names(remove_data_vect[[1]])
names(remove_data_vect)
names(remove_data_vect) <- trend_names
length(remove_data_vect)
length(remove_data_vect[[1]])
length(remove_data_vect[[2]])
length(remove_data_vect[[3]])

# Plot the results to see how the data remotion was done
plot(as.numeric(as.matrix(remove_data_vect[[3]][[4]][[1]][1:100, ]))) ##Linear with less 80
plot(as.numeric(as.matrix(remove_data_vect[[3]][[1]][[1]][1:100, ]))) ##Linear with less 20
plot(as.numeric(as.matrix(remove_data_vect[[3]][[5]][[1]][1:100, ]))) ##Linear with less 95

## Computes the LPI in all of the matrices
############################################ 
RemovingData_results <- vector("list", length(remove_data_vect))
route<-'03processedData/complete/simulated/Conv_conc_lin_Remdt/'

for (i in seq_along(remove_data_vect)) {
  trend_block <- vector("list", length(remove_data_vect[[i]]))
  for (j in seq_along(remove_data_vect[[i]])) {
    rep_block <- vector("list", n_rep)
    
    for (r in seq_len(n_rep)) {
      x.j <- as.data.frame(remove_data_vect[[i]][[j]][[r]])
      x.j <- x.j[1:10, ]   # testing
      
      rep_block[[r]] <- LPIMain(
        create_infile(
          x.j,
          index_vector = TRUE,
          name = paste0(route, trend_names[i], "_remove",red_values[j], "_rep", r),
          start_col_name = "X1950", end_col_name   = "X2020", CUT_OFF_YEAR   = 1950),
        title = paste0( trend_names[i],". Removing ", red_values[j], " of Data (rep ", r, ")"),
        REF_YEAR = 1950,
        PLOT_MAX = 2020,
        BOOT_STRAP_SIZE = 1000,
        VERBOSE = FALSE)}
    trend_block[[j]] <- rep_block
  }
  RemovingData_results[[i]] <- trend_block
}

####Matrix with final data 
# Create a list to store the subsets
dir.create("04FinalData/complete/simulated/Conv_conc_lin_Remdt/",
           recursive = TRUE,
           showWarnings = FALSE)

Mnames = c('20%', '40%', '60%', '80%', '95%')

Remresu_Join <- map_df(seq_along(RemovingData_results), function(trend_idx) {
  trend_list <- RemovingData_results[[trend_idx]]
  trend_type <- trend_names[trend_idx]
  
  map_df(seq_along(trend_list), function(cat_idx) {
    
    cat_list <- trend_list[[cat_idx]]        
    missing_pct <- Mnames[cat_idx] 
    category_name <- names(trend_list)[cat_idx]
    
    map_df(seq_along(cat_list), function(rep_idx) {
      df <- cat_list[[rep_idx]] %>%
        mutate(
          sim = rep_idx,
          label = category_name,
          label = missing_pct,
          trend_type = trend_type,
          years = 1950:2021 )
      df
    })
  })
})

write.csv(Remresu_Join, '04FinalData/complete/simulated/Conv_conc_lin_Remdt/RemovingData_results.csv')
#Remresu_Join<- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/RemovingData_results.csv')

MeanRemresu_Join <- Remresu_Join %>%
  group_by(trend_type, label, years) %>%
  summarise(
    CI_high  = median(CI_high, na.rm = TRUE),
    CI_low   = median(CI_low,  na.rm = TRUE),
    LPI_final = median(LPI_final, na.rm = TRUE),
    .groups = "drop" ) %>%
  mutate(sim = "median")

write.csv(MeanRemresu_Join, '04FinalData/complete/simulated/Conv_conc_lin_Remdt/RemovingData_resultsMedian.csv')

Concave_miss_data  <-MeanRemresu_Join %>% filter(trend_type == "Concave Decrease")
Concave_miss_data <- Concave_miss_data[Concave_miss_data$years != 2021, ]# removing the “2021” value that is just a duplicate placeholder
write.csv(Concave_miss_data, '04FinalData/complete/simulated/Conv_conc_lin_Remdt/Conv_gapsMed.csv')

linear_miss_data <- MeanRemresu_Join %>% filter(trend_type == "Linear Decrease" )
linear_miss_data <- linear_miss_data[linear_miss_data$years != 2021, ]# removing the “2021” value that is just a duplicate placeholder
write.csv(linear_miss_data, '04FinalData/complete/simulated/Conv_conc_lin_Remdt/linear_gapsMed.csv')

convex_miss_data <- MeanRemresu_Join %>% filter(trend_type == "Convex Decrease")
convex_miss_data <- convex_miss_data[convex_miss_data$years != 2021, ]# removing the “2021” value that is just a duplicate placeholder
write.csv(convex_miss_data, '04FinalData/complete/simulated/Conv_conc_lin_Remdt/convex_gapsMed.csv')

#Concave_miss_data <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/Conv_gapsMed.csv')
#linear_miss_data <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/linear_gapsMed.csv')
#convex_miss_data <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/convex_gapsMed.csv')

############
### END ####
############