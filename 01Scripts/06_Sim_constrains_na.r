# =============================================================================
# Large-scale simulation workflow
#
# This script was developed for execution on Calcul Québec (calculquebec.ca)  
# and the Digital Research Alliance of Canada (alliancecan.ca) 
# resources using the SLURM scheduler.
#
# The simulation workload is computationally intensive and may require
# substantial CPU time and memory. 
#
# Nevertheless, this script can detect if you are runnign the code locally.
# 
# If you are working in HPC (for  Calcul Québec (calculquebec.ca))
#    - Run via SLURM job array
#    - Iteration is provided as an argument:
#        Rscript script.R $SLURM_ARRAY_TASK_ID
#
# If you are working at local execution, select the number of iteractions, you want to run
#   Required resources (local execution):
#    - ≥16 CPU cores recommended
#    - ≥64 GB RAM recommended for full workflow
# =============================================================================

library(tidyverse)
library(rlpi)
library(data.table)

#functions 
source('01Scripts/Functions.r')

## Unified local + HPC execution

args <- commandArgs(trailingOnly = TRUE)

# Detect execution mode
is_hpc <- length(args) > 0

if (is_hpc) {

  # HPC mode: one iteration per job
  iteration_number <- as.numeric(args[1])

  message("[MODE] HPC")
  print(paste("Simulation starts at", Sys.time()))

  process_permutation(
    w = iteration_number,
    base_path = "03processedData/constrain/2_na_permutations/simulatedData",
    title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA permutation"
  )

  print(paste("Iteration", iteration_number, "finished at", Sys.time()))
  print("Permutations with the missing dataset finnished")

} else {

  # Local mode: run all permutations sequentially
  iteration_number <- seq(1, 2)

  message("[MODE] LOCAL")
  warning("This code is running only ", length(iteration_number), " iteractons. Change it if you want to run more")

  for (v in iteration_number) {

    print(paste("Simulation starts at", Sys.time()))

    process_permutation(
      w = v,
      base_path = "03processedData/constrain/2_na_permutations/simulatedData",
      title_prefix = "LPI Results Simulated Data - Real Dataset - Only NA permutation"
    )

    print(paste("Iteration", v, "finished at", Sys.time()))
  }

  print("Permutations with the missing dataset finnished")
}

##############
#### END #####
##############  