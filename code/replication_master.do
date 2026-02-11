* Cagé, Le Pennec, Mougin (2023)
* Replication by: Andriyanto, Eugster, Huang, Kalayci (2024)
* Replication Master
*
* INSTRUCTIONS:
*   1. Set $dir below to the root of this repository on your machine.
*   2. Place the original ICPSR data (openicpsr-184946) in the data/ folder
*      following the structure described in README.md.
*   3. Run this file.

* >>>  SET THIS PATH  <<<
global dir "/CHANGE/THIS/PATH/TO/YOUR/LOCAL/CLONE"

set matsize 800

********************************************************************************
* Set path to working directories:
********************************************************************************

global raw_data "$dir/data/raw"
global output "$dir/data/output"
global temp "$dir/data/temp"
global dofiles "$dir/code"
global des "$dir/code/descriptive_stats"
global text "$dir/data/raw/outcomes"
global main "$dir/output/main_paper"
global appendix "$dir/output/appendix"

********************************************************************************
* Install ados / packages:
********************************************************************************
cap ssc install estout
cap ssc install esttab
cap ssc install eststo
cap ssc install estadd
cap ssc install estpost
cap ssc install cibar
cap ssc install ftools
cap ssc install reghdfe
cap ssc install reclink
cap ssc install coefplot
cap ssc install winsor2
********************************************************************************
* Create main dataset
********************************************************************************

*** STEP 1 : create dataset and merge with donation dataset
do "$dofiles/create_dataset/step1_create_candidates_revenues.do"

*** STEP 2 : create dataset of outcomes of interest
do "$dofiles/create_dataset/step2_create_outcomes_dataset.do"
 
*** STEP 3 : merge data on revenues and text outcomes
do "$dofiles/create_dataset/step3_merge_w_candidates.do"

 
********************************************************************************
* Descriptive statistics
********************************************************************************
do "$dofiles/descriptive_stats/descriptive_statistics.do"

********************************************************************************
* Run analysis
*** main paper (baseline)
do "$dofiles/results/main_results.do"
*** main paper (with p-values)
do "$dofiles/results/main_results_p.do"
*** main paper (winsorized at 1st/99th percentiles)
do "$dofiles/results/main_results_winzor.do"
*** main paper (winsorized at 5th/95th percentiles)
do "$dofiles/results/main_results_winzor5.do"
*** main paper (positive-donation candidates only)
do "$dofiles/results/main_results_positive.do"
*** robustness coefficient comparison figure
do "$dofiles/results/figure_robustness_coefplot.do"
*** appendix
do "$dofiles/results/appendix_results.do"

********************************************************************************
* Online Appendix
********************************************************************************
* descriptive tables and figures
do "$dofiles/descriptive_stats/appendix_des_statistics.do"

