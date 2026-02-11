# Replication Report: Cagé, Le Pennec & Mougin (2024)

**"Firm Donations and Political Rhetoric: Evidence from a National Ban"**
*American Economic Journal: Economic Policy*

## Replication Team

Andriyanto (corresponding author), Nicolas Eugster, Guan-Jia Huang, Kenan Kalayci

School of Economics, The University of Queensland

Prepared for the [Institute for Replication (I4R)](https://i4replication.org/) Replication Games 2024.

## Overview

This repository contains the replication report and code for our replication of Cagé, Le Pennec & Mougin (2024). The replication includes:

1. **Computational reproducibility** of all main tables and figures from the original paper.
2. **Robustness checks** including winsorization, alternative control specifications, decomposed mandates, and restriction to positive-donation candidates.
3. **A new robustness figure** (coefficient comparison plot) summarising the sensitivity of Table 3 results across seven specifications.

## Repository Structure

```
├── README.md
├── .gitignore
├── report/                    # Replication report (LaTeX source + compiled PDF)
│   ├── main.tex
│   ├── main.pdf
│   ├── biblio.bib
│   ├── MJOARTI.STY
│   ├── Tables/                # LaTeX table files referenced in the report
│   ├── Figure_robustness.pdf
│   ├── Figure2a.pdf
│   ├── Figure2b.pdf
│   └── Figure3.pdf
├── code/                      # Stata do-files
│   ├── replication_master.do  # Master script — run this file
│   ├── _labels.do
│   ├── create_dataset/        # Data construction (Steps 1–3)
│   ├── descriptive_stats/     # Descriptive statistics and appendix figures
│   └── results/               # Main results, robustness, and figures
├── output/                    # Generated tables and figures
│   ├── main_paper/            # Main paper tables (.tex, .txt) and figures (.pdf)
│   └── appendix/              # Appendix tables and figures
└── data/                      # DATA NOT INCLUDED — see instructions below
    ├── raw/
    │   ├── revenues/
    │   ├── elections/
    │   ├── outcomes/
    │   └── controls/
    ├── output/
    └── temp/
```

## Data Availability

The original data are provided by Cagé, Le Pennec & Mougin through the AEA Data and Code Repository on ICPSR:

> Cagé, Julia, Le Pennec, Caroline, and Mougin, Elisa. Data and Code for: "Firm Donations and Political Rhetoric: Evidence from a National Ban." Nashville, TN: American Economic Association [publisher], 2023. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor]. https://doi.org/10.3886/E184946V1

**To obtain the data:**

1. Visit [https://www.openicpsr.org/openicpsr/project/184946/version/V1/view](https://www.openicpsr.org/openicpsr/project/184946/version/V1/view)
2. Download and unzip the replication folder.
3. Copy the contents of `1.data/raw/` into `data/raw/` in this repository, preserving the subfolder structure (revenues, elections, outcomes, controls).

The `data/output/` and `data/temp/` folders will be populated automatically when the code runs.

## Software Requirements

- **Stata 17.0** (or later)
- The following Stata packages are installed automatically by the master do-file:
  `estout`, `esttab`, `eststo`, `estadd`, `estpost`, `cibar`, `ftools`, `reghdfe`, `reclink`, `coefplot`, `winsor2`

## Instructions to Replicate

1. Clone this repository.
2. Download the original data from ICPSR (see above) and place it in `data/raw/`.
3. Open `code/replication_master.do` in Stata.
4. Change the `global dir` path on line 11 to point to your local clone of this repository.
5. Run `code/replication_master.do`.

All tables and figures will be saved to `output/main_paper/` and `output/appendix/`.

## Do-File Descriptions

| File | Description |
|------|-------------|
| `replication_master.do` | Master script that sets paths and runs all other do-files |
| `create_dataset/step1_create_candidates_revenues.do` | Constructs candidate-revenue dataset |
| `create_dataset/step2_create_outcomes_dataset.do` | Constructs outcome variables from text analysis |
| `create_dataset/step3_merge_w_candidates.do` | Merges revenues with candidate and outcome data |
| `descriptive_stats/descriptive_statistics.do` | Main paper descriptive statistics |
| `descriptive_stats/appendix_des_statistics.do` | Appendix descriptive statistics and figures |
| `results/main_results.do` | **Tables 3–7** (baseline specification) |
| `results/main_results_p.do` | Tables 3–7 with p-values |
| `results/main_results_winzor.do` | Tables 3–7 winsorized at 1st/99th percentiles |
| `results/main_results_winzor5.do` | Tables 3–7 winsorized at 5th/95th percentiles |
| `results/main_results_positive.do` | Tables 3–4 restricted to positive-donation candidates |
| `results/figure_robustness_coefplot.do` | **Robustness coefficient comparison figure** (new) |
| `results/appendix_results.do` | Appendix tables |

## New Contributions

The following files were created or modified by the replication team:

- `results/figure_robustness_coefplot.do` — **New.** Generates a three-panel forest plot comparing Table 3 coefficients across seven robustness specifications (original, winsorized, no controls, no candidate controls, decomposed mandates, number of mandates, positive donors only).
- `results/main_results_positive.do` — From original authors; included in master execution.
- `results/main_results_winzor.do` — **Modified.** Fixed dataset reference (`analysiswin` → `analysis`).
- `results/main_results_winzor5.do` — **Modified.** Fixed dataset reference (`analysiswin5` → `analysis`).
- `replication_master.do` — **Modified.** Updated paths for portability; added all do-files to execution order; uncommented package installation.

## Runtime

The original authors report the full replication runs in under 10 minutes on an Intel Core i7-8565U (Windows 10, 16GB RAM). The robustness figure adds approximately 2–3 minutes.

## License

The replication code in this repository is provided for academic research purposes. The original data and code are subject to the terms of the ICPSR data use agreement.
