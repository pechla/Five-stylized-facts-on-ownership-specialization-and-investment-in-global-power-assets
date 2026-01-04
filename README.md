## Replication Code: Five Empirical Facts on Ownership, Specialization, and Investment in Global Power Assets

### Overview

This repository contains the replication code for:

**Pichler, A. (2026). "Asset ownership, technological specialization and firm-level transition dynamics: Five empirical facts."**

The paper establishes five empirical facts about corporate ownership, technological specialization, and investment behavior in the global power sector using data on more than 150,000 generation assets owned by approximately 24,000 firms worldwide (2001–2024).

### Five Empirical Facts

1. **Power asset ownership is highly concentrated.** Eighty percent of global installed capacity is held by just three percent of firms.
2. **Firms exhibit extreme technological specialization.** Approximately 80 percent of firms own assets in only a single technology.
3. **Investment dynamics are slow and path-dependent.** A firm's past investment behavior is the strongest predictor of its future technology choices.
4. **Corporate energy transitions are exceedingly rare.** About 3.5% of fossil-dominated firms have transitioned to renewable-majority portfolios.
5. **Transition pathways are idiosyncratic.** Firms follow diverse trajectories with no characteristic technology sequences.

### Data Availability

The primary data were obtained under license from S&P Global Market Intelligence (Capital IQ Pro Energy) and cannot be shared directly.

Supplementary files containing S&P entity and unit identifiers for dataset reconstruction are available from the author upon request. Use of these files requires a valid S&P Capital IQ license.

### Repository Structure

```text
├── build_database/              # Scripts to construct the analysis datasets from raw CIQ exports
│   ├── 0_get_all_owners.R
│   ├── 1_build_basic_data.R
│   ├── 2_company_aggregation.R
│   ├── 3_build_regression_data.R
│   ├── 3X_build_regression_data_helper.R
│   └── master_data_builder.R    # Master script to build all processed data
├── data_CIQ/                    # Raw Capital IQ exports (not included; see Data Availability)
│   ├── all_plants_by_owner.xlsx
│   ├── all_units_by_owner.xlsx
│   ├── owners_information.xlsx
│   └── tech_detail_unit_level-by_unit.xlsx
├── data_CIQ_scripts/            # Documentation and formulas for CIQ data extraction
│   ├── CIQ_formulas.xlsx
│   └── ciq-formulas-readme.txt
├── data_processed/
│   ├── data_temp/               # Intermediate data used during database construction
│   ├── Ember/                   # External Ember data inputs
│   └── R-data-output/           # Final processed datasets used in the analysis
├── offline_source/              # Additional PDFs and figures used for illustration
├── R-code/                      # Main analysis and figure-generation code
│   ├── create_figs_main/        # Scripts to reproduce figures in the main text
│   │   ├── code_fig0/
│   │   ├── code_fig1/
│   │   ├── code_fig2/
│   │   ├── code_fig3/
│   │   └── fig/                 # Generated main-text figures
│   ├── create_figs_SI/          # Scripts and outputs for Supplementary Information figures
│   │   └── fig/                 # Generated SI figures
│   ├── investment_hurdle_model/ # Decision trees and regression analysis for investment dynamics
│   │   ├── 1_decision_trees/
│   │   ├── 2_logistic_regressions/
│   │   ├── 3_linear_regressions/
│   │   └── fig/                 # Figures for the hurdle model
│   └── source_code/
│       └── libraries.R          # Central list of required R packages
├── empirical_investment_dynamics_git.Rproj
├── LICENSE
└── README.md
```

### Replication Instructions

1. **Obtain data access**
   - Obtain access to S&P Capital IQ Pro Energy.
   - Contact the author for supplementary Excel files containing entity and unit identifiers (subject to S&P licensing conditions).
2. **Recreate the raw data extracts**
   - Use the Excel formulas in `data_CIQ_scripts/CIQ_formulas.xlsx` to extract the required data from S&P Capital IQ.
   - Export the data from S&P and save the files in `data_CIQ/` using the filenames referenced in the scripts in `build_database/`.
3. **Build the processed datasets**
   - Open `build_database/master_data_builder.R` and run the script.
   - This script sequentially calls `0_get_all_owners.R`, `1_build_basic_data.R`, `2_company_aggregation.R`, `3_build_regression_data.R`, and `3X_build_regression_data_helper.R` and writes intermediate and final data to `data_processed/`.
4. **Reproduce the figures and tables**
   - Use the scripts in `R-code/create_figs_main/` to reproduce the main-text figures.
   - Use the scripts in `R-code/create_figs_SI/` to reproduce Supplementary Information figures and tables.
   - Use the scripts in `R-code/investment_hurdle_model/` to reproduce the results of the investment hurdle model (decision trees, logistic regressions, and linear regressions).

### Requirements

- **R**: version 4.0 or higher is recommended.
- **R packages**: see `R-code/source_code/libraries.R` for a complete list of required packages.

Install the listed packages before running the replication scripts.

### Citation

If you use this code, please cite:

```bibtex
@article{pichler2026facts,
  title   = {Asset ownership, technological specialization and firm-level transition dynamics: Five empirical facts},
  author  = {Pichler, Anton},
  journal = {},
  year    = {2026}
}
```

### License

The analysis code in this repository is released under the **MIT License**. See `LICENSE` for details.


