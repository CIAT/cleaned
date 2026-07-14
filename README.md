# CLEANED Package Development Work (2025)

This fork documents the development work conducted for the CLEANED package in 2025. All changes made during the year are captured in the commit history.

Full commit history:

[view the 2025 development commits](https://github.com/CIAT/cleaned/compare/staging...M-Emmanuel:cleaned-staging-v2:feature/correct-functions)

**✅ Rationale and Summary of Changes**

**🛠 Logic & Bug Fixes**

- Nitrogen fixation logic was corrected to ensure consistent identification of legume categories across different user input formats, to prevent misclassification and ensure reproducible results.
- Livestock category and productivity logic was updated to select the appropriate methane emission factor and improve the accuracy of greenhouse gas emission estimates.
- Fertilizer rate calculation was corrected by using percentage_n instead of fraction, to ensure consistency between input definitions and applied calculations.
- Meat production estimation logic was updated to improve numerical consistency of output estimates.

**🔄 Variable Harmonization**

Variable names were standardized across functions to align with the JSON output structure:

- *'cp_pregnancy → cp_lys_pregnancy'*
- *'cp_growth → cp_lys_growth'*
- *'fat_content → fat_milkcontent'*
- *'n_content → n_manure_content'*

These changes were implemented to ensure consistency between internal calculations and exported model outputs.

**🧹 Codebase Cleanup**

Redundant or unused variables were removed:

- *'n_fixation, cp_lactation, de, grazing_displacement_energy, egg_energycontent, milk_production, live_weight_gain, climate_2'*

This cleanup was performed to reduce ambiguity, minimize maintenance overhead, and avoid unintended variable reuse.

**➕ New & Updated Outputs**

- Crude protein requirement calculations for lactating small ruminants were added, to improve representation of nutrient requirements in small ruminant systems.
- *'animal_manure_produced'* was included in annual outputs.
  
