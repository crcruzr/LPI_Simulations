
## Functions used in the LPI analyses ##

###########################################
# Function to simulate population growth ##
############################################

# N0 represent the initial population
# r indicates the intrinsic growth rate
# K is the carrying capacity
# gen refers the number of generations 
# stochastic_r and stochastic_K indicate if r and K are stochastic. both varies based in the normal distribution
# sigma_r Variation in intrinsic growth rate
# sigma_K Variation in carrying capacity

pop_growth <- function(N0 = NULL, r = NULL, K = NULL, gen, stochastic_r = FALSE, stochastic_K = FALSE, plotting = FALSE) {
  
  # Set default values for parameters
  if (is.null(r)) r <- runif(1, 0.05, (0.45+0.1)) # Large mammal (T=20 yr)	Mammal	~0.05	Dillingham et al. 2016 (10.1890/14-1990) / Bonnethead shark (Sphyrna tiburo)	Elasmobranch	~0.45	Pardo et al. 2016 (10.1002/ece3.9441) 
  if (is.null(K)) K <- runif(1, 10, 72000) #Elk (Cervus elaphus)	Large mammal	~<10 per herd equilibrium Koetke LJ, et al., (2020). (10.1038/s41598-020-72843-5) / Chinook salmon ~71,844 per delta (historic 75th percentile)	Hall J, et al (2023). (10.1007/s12237-023-01185-y)
  if (is.null(N0)) N0 <- 1
  
  # Initialize population size vector
  pop_size <- numeric(gen + 1)
  pop_size[1] <- N0
 
  for (i in 1:gen) {
    # If stochasticity in r is enabled, apply log-normal environmental noise
    r_curr <- if (stochastic_r) r * exp(rnorm(1, 0, 0.15)) else  r
    
    # If stochasticity in K is enabled, allow environmental variation in carrying capacity
    # The log-normal form ensures K always stays positive
    K_curr <- if (stochastic_K)  K * exp(rnorm(1, 0, 0.25)) else K
    
    # Prevent biologically impossible values caused by extreme draws
    r_curr <- max(r_curr, 0.001)
    K_curr <- max(K_curr, 5)
    
    # Update population size using a Ricker-type density-dependent model
    # Growth slows as population approaches carrying capacity
    pop_size[i + 1] <- round(pop_size[i] * exp(r_curr * (1 - pop_size[i] / K_curr)))
  }
  
  # Plot results if requested
  if (plotting) {
    plot(0:gen, pop_size, type = 'l', col = 'blue', ylim = c(0, max(pop_size)),
         xlab = 'Generation', ylab = 'Population Size', main = 'Population Growth Simulation')
    legend('bottomleft', legend = c('Population'), inset = c(0.05, 0.05),
           col = c('blue'), lty = 1)
  }
  return(pop_size)
}


#############################################
#Function to adjust the format as numeric ##
############################################

clean_data <- function(data) {
  data[] <- lapply(data, function(x) {
    # Step 1: Remove double quotes (keeping "NULL" as it is)
    x <- ifelse(x == "NULL", x, gsub('^"|"$', '', x))
    
    # Step 2: Replace "NULL" with NA and convert other values to numeric
    ifelse(x == "NULL", NA, as.numeric(x))
  })
  return(data)
}

#######################################################
## Function to add mandatory columns to use the LPI ###
#######################################################

bino_id <- function(data, years, num_species) {
  # Add a 'Binomial' column with 'Species1', 'Species2', ..., 'SpeciesN'
  data[, (length(years) + 1)] <- paste0('Species', 1:num_species)
  
  # Set the column names, including 'Binomial' as the last column
  colnames(data) <- c(paste0('X', years), 'Binomial')
  
  # Add an 'ID' column (unique row identifier)
  data$ID <- 1:nrow(data)
  
  return(data)
}

############################################################################
## Function to permute data matrix while preserving NA and zero positions ##
############################################################################

permutationLPI <- function(mat, nperm = 100, shuffle_zeros = TRUE, shuffle_NA = FALSE, years = NULL, S = NULL, summary = NULL) {
  
  # Check if both shuffle options are FALSE
  if (!shuffle_zeros && !shuffle_NA) {
    message("The zero and missing data are marked as FALSE. The matrix will remain unchanged. Shuffling cancelled.")
    return(replicate(nperm, mat, simplify = FALSE))
  }
  
  # Print summary if requested
  if (!is.null(summary) && summary == TRUE) {
    cat("\n=== PERMUTATION SUMMARY ===\n")
    cat("Configuration:\n")
    cat("- nperm:", nperm, "\n")
    cat("- shuffle_zeros:", shuffle_zeros, "\n")
    cat("- shuffle_NA:", shuffle_NA, "\n")
  }

  permuted_matrices <- vector("list", nperm)
pb <- txtProgressBar(min = 0, max = nperm, style = 3)
  on.exit(close(pb))
  
  for (i in 1:nperm) {
    permuted_mat <- t(apply(mat, 1, function(row) {
            na_positions <- is.na(row)
      zero_positions <- !is.na(row) & row == 0

      # Determine which positions are fixed depending on arguments
      fixed_positions <- logical(length(row))

      if (!shuffle_zeros) {
        fixed_positions <- fixed_positions | zero_positions
      }
      if (!shuffle_NA) {
        fixed_positions <- fixed_positions | na_positions
      }

      # Values to shuffle: all positions not fixed
      values_to_shuffle <- row[!fixed_positions]

      # Shuffle values
      shuffled_values <- sample(values_to_shuffle)

      # Create new row and place fixed values and shuffled values accordingly
      new_row <- row
      new_row[!fixed_positions] <- shuffled_values

      return(new_row)
    }))

# Apply bino_id transformation if years and S are provided
    if (!is.null(years) && !is.null(S)) {
    permuted_mat <- bino_id(as.data.frame(permuted_mat), years, S)
    } else {
      permuted_mat <- shuffled_mat
    }
    
     permuted_matrices[[i]] <- permuted_mat
     setTxtProgressBar(pb, i)
  
    # Validation (only for first iteration)
    if (i == 1 && !is.null(summary) && summary == TRUE) {
      cat("\n")
      # Always show validation status regardless of bino_id
      if (!shuffle_NA) {
        cat("- NA positions PRESERVED: TRUE\n")
      } else {
        cat("- NA positions CHANGED: TRUE\n")
      }
      
      if (!shuffle_zeros) {
        cat("- Zero positions PRESERVED: TRUE \n")
      } else {
        cat("- Zero positions CHANGED: TRUE \n")
      }
    }
  }
  return(permuted_matrices)
  }

##################################
## Function to plot LPI results ##
##################################

plot_lpi_table <- function(data, colr, label_name = "LPIX", show_label = TRUE) {
  if (!"years" %in% colnames(data)) {
    stop("The data must contain a 'years' column")
  }
  
# Add a new column with the label_name for mapping fill and color
    
  if (!"label" %in% names(data) | (length(unique(data$label)) == 1)) {
  data$label <- label_name
  }

  p <- ggplot(data, aes(x = years, group = label)) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), alpha = 0.2) +
    geom_hline(yintercept = 1, linetype = "solid", size = 0.5, color = "#000000") +
    geom_line(aes(y = LPI_final, color = label), size = 1) +
    scale_fill_manual(name = NULL, values = colr) +
    scale_color_manual(name = NULL, values = colr) +
    labs(x = "Year", y = "Index", title = "") +
    scale_x_continuous(breaks = seq(1950, 2020, by = 10), expand = c(0.03, 0.05)) +
    scale_y_continuous(limits = c(0, 2), expand = c(0.05, 0.002)) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    geom_vline(xintercept = 1950, color = "gray80", size = 0.5) +
    geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.title = element_text(size = 25),
      axis.text.x = element_text(size = 20, hjust = 0.5, margin = margin(t = 5), angle = 0),
      axis.text.y = element_text(size = 20),
      axis.ticks.x = element_line(size = 0.8, color = "black"),
      axis.ticks.y = element_line(size = 0.8, color = "black"),
      legend.text = element_text(size = 25),
      legend.position = c(0.75, 0.8),
      panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
      panel.grid.major.x = element_line(size = 0.4, color = "gray90"))
  
  if (show_label == FALSE) {
    p <- p + theme(legend.position = "none")
    
  } else if (show_label & (length(unique(data$label)) == 1)) {
    p <- p + guides(
      color = guide_legend(
        override.aes = list(fill = colr[2], color = colr[1], size = 15, alpha = 0.2))
    )
  } else {
    p <- p 
    
  }

  return(p)
}

#########################################################
## Function to plot for data obtained medianted a list ##
#########################################################
lpi_multiplot <- function(data, colr = c("#1f77b4", "#ff7f0e"), validate_simulations = TRUE) {
# Usage:
# plot_result <- create_lpi_plot(your_data, colr = c("#1f77b4", "#ff7f0e"))
  
  # Validation: Check if there's more than one simulation
if (validate_simulations) {
    n_sims <- length(unique(data$sim))
    if (n_sims <= 1) {
      warning("Only one or zero simulations found. Consider use the plot_lpi_table function.")
    }
    message(paste("Number of simulations:", n_sims))
  }

   # Create the plot
  pglpi <- ggplot(data, aes(x = years, y = LPI_final, group = sim)) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high, fill = label), 
                alpha = 0.2, color = NA) +
    geom_line(aes(color = label), size = 0.1) +
    scale_fill_manual(name = NULL, 
                      values = setNames(colr[2], unique(data$label))) +
    scale_color_manual(name = NULL, 
                       values = setNames(colr[1], unique(data$label))) +
    geom_hline(yintercept = 1, linetype = "solid", size = 0.5, color = "black"  ) +
    coord_cartesian(ylim = c(0.0, 2)) +
    scale_x_continuous(breaks = seq(1950, 2020, by = 10), expand = c(0.03, 0.05)) +
    scale_y_continuous(limits = c(0, 2), expand = c(0.05, 0.002)) +
    guides(color = guide_legend(override.aes = list(fill = colr[2], 
                                                    color = colr[1], 
                                                    size = 15, 
                                                    alpha = 0.2))) +
    labs(x = "Year", y = "Index", title = "") +
    geom_vline(xintercept = 1950, color = "gray80", size = 0.5) +
    geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.title = element_text(size = 25),
      axis.text.x = element_text(size = 20, hjust = 0.5, margin = margin(t = 5), angle = 0),
      axis.text.y = element_text(size = 20),
      axis.ticks.x = element_line(size = 0.8, color = "black"),
      axis.ticks.y = element_line(size = 0.8, color = "black"),
      legend.text = element_text(size = 25),
      legend.position = "none",
      panel.grid.major.y = element_line(size = 0.4, color = "gray80"),
      panel.grid.major.x = element_line(size = 0.4, color = "gray90")
    )
  
  return(pglpi)
}

######################################
#function toparalelize theprocesess ##
#######################################
process_permutation <- function(w = 1, base_path = getwd(), 
                                title_prefix = "LPI Results") {
  
  # Define input and output directories
  input_path <- file.path(base_path, "processing")
  output_dir <- file.path(base_path, "results")
  
  # Ensure the results directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load the matrix
  mat <- readRDS(file.path(input_path, sprintf("matrix_%03d.rds", w)))
  
  result <- LPIMain(
    create_infile(
      mat,
      index_vector = TRUE,
      name = file.path(input_path, as.character(w)),
      start_col_name = "X1950",
      end_col_name = "X2020",
      CUT_OFF_YEAR = 1950
    ),
    title = paste(title_prefix, w),
    REF_YEAR = 1950,
    PLOT_MAX = 2020,
    BOOT_STRAP_SIZE = 10,
    VERBOSE = FALSE,  
    SHOW_PROGRESS = TRUE
  )
  
  message("Completed permutation ", w)
  saveRDS(result, file = file.path(output_dir, sprintf("permutation_result_%03d.rds", w)))
  
  return(result)
}