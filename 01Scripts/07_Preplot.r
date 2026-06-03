library(data.table)
library(mutate)
library(map_df)

####### Move files to the final folder
################## 

# Copy the NA permutations with the final result to the Final Data folder
file.copy("03processedData/constrain/2_na_permutations/simulatedData/results/", 
          "04FinalData/constrain/2_na_permutations/simulatedData/",
          recursive = TRUE
          )


# Copy final  zero permutations result to the Final Data folder
file.copy("03processedData/constrain/3_zero_permutations/simulatedData/results/", 
          "04FinalData/constrain/3_zero_permutations/simulatedData/",
          recursive = TRUE
          )

### Zeros on the permutations ###

# Simulated data used to creates the matrices
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
# Check the number of zeros on the results
head(matricesPermu0[[1]],1)

# Adding zeros per permutation inthemaatriz of the results
for (i in seq_along(resultsPermu0)) {
  # Check if the data frame exists and is not NULL
  if (!is.null(resultsPermu0[[i]]) && nrow(resultsPermu0[[i]]) > 0) {
    # Check if corresponding matrix exists in the other list
    if (i <= length(matricesPermu0_up) && !is.null(matricesPermu0_up[[i]])) {
      df_matrix <- matricesPermu0_up[[i]] ## upload the original dataset
      setDT(resultsPermu0[[i]]) # upload the seults
      #resultsPermu0[[i]] <- resultsPermu0[[i]][c(1:nrow(resultsPermu0[[i]]) -1), ]
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

write.csv(fnumzero, '04FinalData/constrain/3_zero_permutations/simulatedData/Number_of_Zeros_zerpermut.csv')
#fnumzero <- read.csv('04FinalData/constrain/3_zero_permutations/simulatedData/Number_of_Zeros_zerpermut.csv')
