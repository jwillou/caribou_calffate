 # Mulchatna Caribou Calf Fate Analysis

This repository contains the complete analysis pipeline for studying calf fate in the Mulchatna Caribou Herd (MCH) in Alaska. The code examines causes of calf mortality variation across calving grounds, sexes, and years.

Characteristics of neonate predation in the migratory Mulchatna caribou herd in Southwest Alaska
 Dominic Demma1, Renae Sattler1, Janna R. Willoughby2 

1 Alaska Department of Fish and Game, Palmer, Alaska, United States of America 
2 College of Forestry, Wildlife, and Environment, Auburn University, Auburn, Alabama, United States of America


## Overview

1. **Annual Mortality Trends**: How do neonate mortality rates vary by year and calving region?
2. **Capture and Mortality Relationships**: How does the number of calves captured relate to mortality rates across years?
3. **Cause-of-Death Composition**: What are the predominant causes of calf mortality, and how do these vary between regions and years?
4. **Age-at-Death Variation**: Do predator types differ in the age at which calves die?
5. **Regional Comparisons**: Are there differences in mortality causes and age-at-death patterns between eastern and western calving areas?

## Requirements

This project is implemented in R and uses the following R packages:

* `ggplot2`, `ggpubr` – for data visualization
* `dplyr`, `tidyr` – for data manipulatio
* `FSA` – for Dunn’s test and non-parametric comparisons

## Key Analyses

* **Line plots** of annual mortality rates and scaled capture numbers, stratified by calving region.
* **Stacked polygon plots** showing proportional mortality by predator type across years and regions.
* **Wilcoxon and Kruskal-Wallis** tests to evaluate age-at-death differences by cause.
* **Dunn’s post-hoc** tests (Benjamini-Hochberg corrected) to identify specific differences between predator types.

## Outputs

* Dual-axis mortality and capture line plots by region and year.
* Age-at-death comparisons by fate type and predator.
* Statistical summaries and significance tests for age-at-death by cause.

