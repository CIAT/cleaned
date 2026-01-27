# CLEANED Package Development Work (2025)

This fork documents the development work conducted for the CLEANED package in 2025. All changes made during the year are captured in the commit history.

Full commit history:

[view the 2025 development commits](https://github.com/CIAT/cleaned/compare/staging...M-Emmanuel:cleaned-staging-v2:feature/correct-functions)

## Summary of Updates

✅ Issues Addressed

**1. Corrected nitrogen-fixation logic** to ensure the “legume” category is detected correctly regardless of user formatting (e.g., “Legume”, “legume”, “tree legume”, “legume tree”, etc.).

**2. Updated variable names** across functions to match the JSON output structure:
   - *'cp_pregnancy → cp_lys_pregnancy'*
   - *'cp_growth → cp_lys_growth'*
   - *'fat_content → fat_milkcontent'*
   - *'n_content → n_manure_content'*

**3. Removed redundant variables** from the codebase:

   - *'n_fixation, cp_lactation, de, grazing_displacement_energy, egg_energycontent, milk_production, live_weight_gain, climate_2'*

**4. Added calculation for crude protein requirements** in lactating small ruminants.

**5. Updated meat production estimation logic** to improve accuracy.

**6. Included** *'animal_manure_produced'* in the annual output results.

**7. Updated livestock category and productivity logic** to select the correct methane emission factor.

**8. Corrected fertilizer rate calculation**, ensuring the correct variable (*'percentage_n'*) is used instead of *'fraction'*.
