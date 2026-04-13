library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(rlpi)
library(data.table)

# Load functions
source('01Scripts/Functions.r')
colr <- c("#9467bd", "#c5b0d5") 
colr2 <- c("#ff7f0e", "#ffbb78") 
colors <- c("#558ed5", "#77933d", "#4a452a") 
colorsG <- c("#558ed5", "#77933d", "#4a452a", "#d87c30", "#5b9aa0")
years <- 1950:2020 ## Modified to add the same number of years in the LPI

### Fig 1 ####
lpi_result <- read.csv('04FinalData/complete/simulated/Complete_dataSet/Complete_dataSet.csv')
f1a <- plot_lpi_table(lpi_result, colr,  show_label = F); f1a
ggsave(filename=paste0("05Plots/Fig1a.jpeg"), f1a, dpi = 300) ## plot used in the paper

f1comb <- read.csv('04FinalData/complete/simulated/Conv_conc_lin/Conv_conc_lin.csv')
f1b <- f1comb %>%
  filter(label == 'concave')
f1b <- plot_lpi_table(f1b, colr = colr, show_label = F);f1b
ggsave(filename=paste0("05Plots/Fig1b.jpeg"), f1b, dpi = 300) ## plot used in the paper

f1c <- f1comb %>%
  filter(label == 'linear')
f1c <- plot_lpi_table(f1c, colr = colr, show_label = F );f1c
ggsave(filename=paste0("05Plots/Fig1c.jpeg"), f1c, dpi = 300) ## plot used in the paper

f1d <- f1comb %>%
  filter(label == 'convex')
f1d <- plot_lpi_table(f1d, colr = colr, show_label = F);f1d
ggsave(filename=paste0("05Plots/Fig1d.jpeg"), f1d, dpi = 300) ## plot used in the paper

plot_data2 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/convex_gapsMed.csv')
f1e <- plot_lpi_table(plot_data2, colr = colorsG, show_label = FALSE); f1e
ggsave(filename=paste0("05Plots/Fig1e.jpeg"), f1e, dpi = 300) ## plot used in the paper

plot_data3 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/linear_gapsMed.csv')
f1f <- plot_lpi_table(plot_data3, colr = colorsG, show_label = FALSE); f1f
ggsave(filename=paste0("05Plots/Fig1f.jpeg"), f1f, dpi = 300) ## plot used in the paper

plot_data1 <- read.csv('04FinalData/complete/simulated/Conv_conc_lin_Remdt/Conv_gapsMed.csv')
f1g <- plot_lpi_table(plot_data1, colr = colorsG, show_label = FALSE); f1g
ggsave(filename=paste0("05Plots/Fig1g.jpeg"), f1g, dpi = 300) ## plot used in the paper


f1 <- (plot_spacer() | f1a| plot_spacer()) /
  (f1b | f1c| f1d) /
  (f1e | f1f| f1g)  &
  plot_annotation(tag_levels = "A")  &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    # Tick labels
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    # Tick marks
    axis.ticks = element_line(size = 0.5),
    # Panel border
    panel.border = element_rect(size = 0.6),
    # Legend
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "mm"),
    legend.key.width  = unit(1, "mm")
  ) &
  guides(
    color = guide_legend(override.aes = list(size = 4))
  )
f1
ggsave(filename=paste0("05Plots/Fig1.jpeg"), f1, dpi = 300) ## plot used in the paper


##Fig 2

lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')
f2a <- plot_lpi_table(lpi_resultR, colr = colr2, show_label = FALSE);f2a
ggsave(filename=paste0("05Plots/Fig2a.jpeg"), f2a, dpi = 300) ## plot used in the paper

lpi_simul_real_temp <- read.csv('04FinalData/constrain/1_na_zero_permutations/without_permutation/without_permutationNAand0.csv')
f2b <- plot_lpi_table(lpi_simul_real_temp, colr = colr, show_label = FALSE, label_name = ""); f2b
ggsave(filename=paste0("05Plots/Fig2b.jpeg"), f2b, dpi = 300) ## plot used in the paper

lpi_resultzero <- read.csv('04FinalData/constrain/3_zero_permutations/without_permutation/without_permutationzero.csv')
f2c <- plot_lpi_table(lpi_resultzero, colr = colr, show_label = FALSE, label_name = "");f2c
ggsave(filename=paste0("05Plots/Fig2c.jpeg"), f2c, dpi = 300) ## plot used in the paper

lpi_resultNA <- read.csv('04FinalData/constrain/2_na_permutations/without_permutation/without_permutationNA.csv')
f2d <- plot_lpi_table(lpi_resultNA, colr = colr, show_label = FALSE, label_name = ""); f2d
ggsave(filename=paste0("05Plots/Fig2d.jpeg"), f2d, dpi = 300) ## plot used in the paper

#Merge all of the iteractions

nf1 <-length(list.files('03processedData/constrain/3_zero_permutations/simulatedData/results/', full.names = TRUE))
resultsPermu0 <- lapply(1:nf1, function(i) {
  filepath <- sprintf("03processedData/constrain/3_zero_permutations/simulatedData/results/permutation_result_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  # or NA, or any placeholder for missing files
  }
})

# Convert each data frame in the list to a data.table and add years column
for (i in seq_along(resultsPermu0)) {
  setDT(resultsPermu0[[i]])  # convert in-place, no warning if already data.table
  resultsPermu0[[i]][, years := c(years,2021)]
}

head(resultsPermu0[[1]],3)

f2e <- purrr::map_df(seq_along(resultsPermu0), ~ mutate(resultsPermu0[[.x]], sim = .x, label = "Permutations using \n empirical-data zeros"))
f2e <- lpi_multiplot(f2e, colr = colr); f2e
ggsave(filename=paste0("05Plots/Fig2e.jpeg"), f2e, dpi = 300) ## plot used in the paper

f2 <- (f2a | f2b) /
  (f2c | f2d) /
  (f2e| plot_spacer())  &
  plot_annotation(tag_levels = "A")  &
  theme(
    plot.tag = element_text(size = 15, face = "bold"),
    # Axis titles
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.title.x = element_text(size = 12),
    #axis.title.y = element_text(size = 12),
    # Tick labels
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    # Tick marks
    axis.ticks = element_line(size = 0.5),
    # Panel border
    panel.border = element_rect(size = 0.6),
    # Legend
    legend.text = element_text(size = 12),
    legend.key.height = unit(1, "mm"),
    legend.key.width  = unit(1, "mm")
  ) &
  guides(
    color = guide_legend(override.aes = list(size = 4))
  )

f2

ggsave(filename=paste0("05Plots/Fig2.jpeg"), f2, dpi = 300) ## plot used in the paper

### Fig 3
lpi_resultNAZero <- read.csv('04FinalData/constrain/1_na_zero_permutations/without_permutation/without_permutationNAand0.csv')
lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')

filter_labels <- function(df) {
  df %>% 
    filter(
      (years >= 1950 & years < 1980 & years %% 5 == 0) | 
      (years >= 1980 & years < 2000 & years %% 10 == 0) | 
      (years >= 2000 & years <= 2020)
    )
}
lpi_labels <- lpi_resultR %>%
  filter(years %in% c(1955, 1974, 1991, 1997, 2009, 2010, 2013, 2017, 2020))



Fig3 <-
 ggplot(data = lpi_resultR, aes(x = numZeros, y = LPI_final, label = years, shape = "Simulations")) +
  geom_hline(yintercept = 1,  color = "orange", size = 1) +#ff7300
  coord_cartesian( xlim = c(0, 3700)) +
  geom_point(aes( fill = years), alpha = 0.7, size = 7, colour="black",pch=21) +
  geom_segment(data = lpi_labels, aes(x = numZeros + 50, y = LPI_final, xend = numZeros + 190, yend = LPI_final), color = "black") +
  geom_text(data = lpi_labels, aes(x = numZeros + 210, y = LPI_final, label = years), hjust = 0, size = 5) +

 # geom_point(data = lpi_resultR, aes( fill = years, shape = "LPD"), alpha = 0.7, size = 5, colour="black",pch=22) +

scale_fill_viridis_c(option = "D",
       guide = guide_colorbar(nbin = 8, raster = FALSE, barheight = unit(5, "cm"), ticks = FALSE, show.limits = FALSE),
       breaks = c(1950, 1960, 1980, 2000, 2020), 
       limits = c(1950, 2020)) +

  labs(x = "Number of Zeros",
       y = "LPI", fill = "Years\n") +

  theme_minimal() +
    theme(
      panel.border = element_rect(
      color = "black",      
      fill = NA,            
      linewidth = 2    
    ),
    text = element_text(size = 20, family = "bold"),
    plot.title = element_text(size = 14, face = "bold") 
  )

Fig3

ggsave(filename=paste0("05Plots/Fig3.png"), Fig3,  width = 35, height = 20, units = "cm", dpi = 300) ## plot used in the paper

##### Supp Mat
## SuppMat 1a
lpi_resultR <- read.csv('04FinalData/complete/real/Complete_dataSet/Complete_dataSet.csv')

SuppMat1 <- ggplot(lpi_resultR, aes(x = years)) +
  coord_cartesian(ylim = c(0, 1.3)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.3, fill = "#ff7f0e") +
  geom_line(aes(y = LPI_final), color = "#ff7f0e", size = 1.5) +
  geom_line(aes(y = numZeros/max(numZeros)*1.2), color = "#9467bd", size = 1.5) +
    geom_hline(yintercept = 1,  color = "#000000a8", size = 1) +#ff7300

  scale_y_continuous(
    name = "LPI values",
    sec.axis = sec_axis(~ . * max(lpi_resultNAZero$numZeros)/1.2, name = "Number of Zeros")
  ) +
  labs(x = "Years") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    text = element_text(size = 20, family = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(color = "#ff7f0e"),
    axis.title.y.right = element_text(color = "#9467bd")
  )

ggsave(filename=paste0("05Plots/SuppMat1.jpeg"), SuppMat1, width = 35, height = 20, units = "cm", dpi = 300) ## plot used in the paper

## SuppMat 1b
## Zeros on the permutations
# simulated data used to creates the matrices
npd <-length(list.files('03processedData/constrain/3_zero_permutations/simulatedData/processing/',  pattern = 'matrix', full.names = TRUE))

matricesPermu0 <- lapply(1:npd, function(i) {
  filepath <- sprintf("03processedData/constrain/3_zero_permutations/simulatedData/processing/matrix_%03d.rds", i)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL  
  }
}) 
# Create a matrix with the matrices used to obtain the LPI
matricesPermu0_up <- lapply(matricesPermu0, function(df) {
  df[ , !(names(df) %in% c('Binomial', 'ID'))]
})

## Check the number of zeros on the results
head(matricesPermu0[[1]],3)

# Adding zeros per permutation inthemaatriz of the results
for (i in seq_along(resultsPermu0)) {
  # Check if the data frame exists and is not NULL
  if (!is.null(resultsPermu0[[i]]) && nrow(resultsPermu0[[i]]) > 0) {
    # Check if corresponding matrix exists in the other list
    if (i <= length(matricesPermu0_up) && !is.null(matricesPermu0_up[[i]])) {
      df_matrix <- matricesPermu0_up[[i]] ## upload the original dataset
      setDT(resultsPermu0[[i]]) # upload the seults
      resultsPermu0[[i]] <- resultsPermu0[[i]][c(1:nrow(resultsPermu0[[i]]) -1), ]
      ## Ad columns
      resultsPermu0[[i]][, years := c(years)] # years
      numZeros <- colSums(df_matrix == 0, na.rm = TRUE) 
      resultsPermu0[[i]] [, numZeros := numZeros] # number of zeros
    }
  }
}

head(resultsPermu0[[2]],3)
# Keep only non-NULL data frames
valid_indices <- which(!sapply(resultsPermu0, is.null) & sapply(resultsPermu0, is.data.frame))

fnumzero <- map_df(valid_indices, ~ {
  mutate(resultsPermu0[[.x]], sim = .x, label = "Permutations")
})

SuppMat2 <- ggplot()+
 coord_cartesian(ylim = c(0.5, 1.3)) +

  geom_point(data = fnumzero, aes(x = numZeros, y = LPI_final), 
             color = "#1f77b4", alpha = 1, size = 1) +

  geom_point(data = lpi_resultNAZero, aes(y = LPI_final, x = numZeros), 
            fill = "#ff7f0e",  size = 3,  colour="black",pch=21) +
  geom_hline(yintercept = 1,  color = "#000000a8", size = 1) +#ff7300
  labs(x = "Number of zeros", y = "LPI values") +
  theme_minimal() +
  theme(
        panel.border = element_rect(
      color = "#000000",      
      fill = NA,            
      linewidth = 2,      
    ),
    text = element_text(size = 20, family = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )
SuppMat2


ggsave(filename=paste0("05Plots/SuppMat2.jpeg"), SuppMat2, width = 35, height = 20, units = "cm", dpi = 300) 

