# =============================================================================
# Large-scale simulation workflow
#
# This script was developed for execution on Compute Canada / Alliance HPC
# resources using the SLURM scheduler.
#
# The simulation workload is computationally intensive and may require
# substantial CPU time and memory. 
#
# This script can be run in two modes:
# 1) HPC (for example Alliance / Compute Canada)
#    - Run via SLURM job array
#    - Iteration is provided as an argument:
#        Rscript script.R $SLURM_ARRAY_TASK_ID
#
# 2) Local execution 
#    - Run manually in a loop or with parallel processing
#    - Example:
#        for i in 1:30; do Rscript script.R $i; done
#    - Or with GNU parallel:
#        seq 1 30 | parallel -j 16 Rscript script.R {}
#
# Required resources (local execution):
#    - ≥16 CPU cores recommended
#    - ≥64 GB RAM recommended for full workflow
# =============================================================================

library(tidyverse)
library(rlpi)
library(data.table)

#functions 
source('01Scripts/Functions.r')

## Run 300 Permutations
args = commandArgs(trailingOnly=TRUE)
iteration_number <- as.numeric(args[1])

print(paste("Simulation starts at", Sys.time()))

# Process to do the iterations
process_permutation(
  iteration_number, 
  base_path = "03processedData/constrain/1_na_zero_permutations/simulatedData",
  title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA and zero - permutation"
)

print(paste("Iteration", iteration_number, "finished at", Sys.time()))
print('Permutations with the zeros and missing dataset finnished')

##############
#### END #####
##############  