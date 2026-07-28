
# Project Overview

This repository contains the scripts, data, and outputs required to reproduce the full analytical pipeline used in the paper **Assessing the sensibility and robustness of the Living Planet Index through simulated population dynamics: Strengths, stability, and challenges**, including simulation, trend modeling, constraint application, and computation of the **Living Planet Index (LPI)**.

The analysis is based on the **Living Planet Database (LPD)** and on simulated population time series designed to evaluate how missing values (`NA`) and zero counts affect the LPI under different trend structures (concave, linear, and convex).

---

# Repository Structure

The repository is organized into the following directories:

## `00RawData`

Contains the **Living Planet Database (LPD)** used as the empirical reference for all analyses.

---

## `01Scripts`

Contains all scripts required to generate simulations, apply constraints, and compute the Living Planet Index.

### `01_SimulatedPop_LPI`

Generates simulated population time series and computes the LPI using both the simulated data and the LPD.

### `02_trends_LPI`

Applies concave, linear, and convex trends to the simulated data and computes the corresponding LPI values.

### `03_SimConstrains_datapreparation`

Prepares the simulated data by introducing `NA` values and zeros following the structure observed in the LPD, and formats the data for permutation and iteration analyses.

### `04_Sim_constrains_na_and_zeros`

Computes the LPI using simulated data with imposed `NA` values and zeros, varying their positions across iterations.

### `05_Sim_constrains_zeros`

Computes the LPI using simulated data with imposed zeros, varying their positions across iterations.

### `06_Sim_constrains_na`

Computes the LPI using simulated data with imposed `NA` values, varying their positions across iterations.

### `07_Preplot`

Prepares part of the data required to create the figures.

### `08_Figs`

Creates the graphs used in the manuscript.

---

## `02Documents`

Directory intended for working documents and notes. Files in this folder are not shared as part of the public repository.

---

## `03processedData`

Stores all intermediate data products generated during the different processing steps and analytical approaches.

---

## `04FinalData`

Contains the final datasets used for analysis and reporting.

---

## `05Plots`

Contains all graphs generated for the manuscript and supplementary materials.

---

# Reproducibility

All analyses can be reproduced by running the scripts in the `01Scripts` directory in numerical order.

Intermediate outputs are stored in `03processedData`, final datasets are written to `04FinalData`, and figures are saved in `05Plots`.

>[!Warning]
 >These simulations use the rlpi package. To install the exact same version we used (SHA 2631ae8), run as follows: 
 >```R
> pak::pak("github::Zoological-Society-of-London/rlpi@2631ae8")
> ```

## Computational Requirements

Scripts 4–6 were developed for execution on [Alliance (Compute Canada)](https://www.alliancecan.ca/en) high-performance computing resources. These simulations are computationally demanding and may not be practical on standard personal computers.

Users with access to Alliance should execute these scripts through SLURM job submissions and adjust the requested resources (e.g., wall time, memory, CPUs, and account allocation) according to their computing environment.

Users attempting local execution should have access to a high-performance workstation with multiple CPU cores and substantial memory (recommended: ≥16 CPU cores and ≥64 GB RAM), and should adjust the parallelization settings accordingly.

### Alliance Users

Scripts 4–6 were designed to be submitted as SLURM jobs. The SLURM configurations included in the scripts should be adapted to the user's Alliance allocation and available computational resources.
```
