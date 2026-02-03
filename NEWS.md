# ageproR 0.9.0 2026-02-00

- Default current version AGEPRO input file format is set to `AGEPRO VERSION 4.25`. 
  - Added `agepro40_inpfile_string` for `AGEPRO VERSION 4.0` compatibility.
  - Improved AGEPRO Input File Format Version validation command line messages 
    - Warning message is more clarified if `AGEPRO VERSION 4.0` was detected.
    - Assert for valid strings and supported input file formats
- Auxiliary Output Flag (`auxfile_output_flag`)
  - All remaining instances of `output_stock_summary` was renamed to `auxfile_output_flag`. 
    - options_output: param `summary_report` -> `auxiliary_flag`
  - Additional Auxiliary Output Flag parameter is added (`valid_aux_output_flags = c(0,1,2,3,4)`) to match AGEPRO-GUI 4.3.6 and December 2025 version of the AGEPRO Reference Manual (#98)
  - Supported Input File format validation is based on "Current Version" and `AGEPRO VERSION 4.0` Input File Format (`enable_agepro40_format`):
    - If `enable_agepro40_format` is TRUE, check if value is _logical_ or (`0`,`1`); if FALSE, checks if value matches `valid_aux_output_flags`.
- Updated **testthat** example to use AGEPRO Calculation Model's 4.25 updated Example 1 Input Model: 
  - Added [AGEPRO Example 1](https://github.com/noaa-pifsc/AGEPRO/tree/main/examples/Example%201) (`AGEPRO VERSION 4.25` input format); 
    - Exported Example 1 Model data to new example JSON file `Example1.json`
  - `test-agepro_model.R`: Added checks to see if **Example1** Input File and Bootstrap File Exists. Removed `set_bootstrap_file` test 
  - Previous example test example file `ageproInput.INP` is replaced by [AGEPRO_GUI Example 4](https://github.com/nmfs-ost/AGEPRO-GUI/tree/master/examples/example4) (`AGEPRO VERSION 4.0`)
    - Added Example 4 Bootstrap File; `Example4_UKU.INP`: Modified **bootstrap_file** to be a relative path.
- **bootstrap_file** relative filepaths can be verified: 
  - Check the path of dirname of the input file, being read via file connection, plus the **bootstrap_file** relative filepath exists. 
    If found save relative filepath to **bootstrap_file**. Otherwise, use **working directory** and use original bootstrap path validation.
  - This has a side effect of printing out absolute paths which affects **testthat** snapshots, that varies the output of the **`bootstrap_file`** causing test snapshot errors.
    - Fixup `test-agepro_model.R` : Substitute **`bootstrap_file`** output value path w/ fixed value `<path>`
- Refactor out `validate_logical_parameter` function from validation.R to as a `options_output` private helper method
- JSON inupt file
  - `ver_json_format` is now 2 (Changes to the JSON input format will increment up +1)
    - Fix inconsistencies (`ver_jsonfile_format` -> `ver_json_format`) causing **NULL** values
  - JSON fields renamed to be more consistent with the _AGEPRO Reference Manual_ Table 3
    - **`summary_output_flag`** **->** **`auxiliary_output_flag`**
    - `process_error_aux_data_flag` -> `process_dataflag`
    - `report_precentile_value` -> `perc_report_value`
    - `biomass_scale` -> `scale_biomass`
    - `recruitment_scale` -> `scale_recruitment`
    - `stock_size_scale` -> `scale_stock_size` 
- Rename 
  - `assert_inpfile_version` -> `check_inpfile_version`
  - parameter `process_error_aux_files` -> `process_error_datafiles`
  - function `output_data_frame` -> `export_df`
  - param `export_r_data_frame` -> `export_df`
- Bugfixes to `read_inp_file` **trycatch** error handling. 
  - cli tweaks to show "error conditions"
  - Suppress `enable_percentile_summary` setup messages when resetting projection analysis types.  
  - Fixed an erroneous call resetting `enable_percentage_summary` value.
- Updated README: Fixes and updated links from nmfs-ost migration (#96, #95)
- Update roxygen to 7.3.3


# ageproR 0.8.0 2025-07-23

- Recruitment 
  - Added `empirical_ssb` for **Empirical Recruit Per Spawning Mass Distribution** (Recruitment model 2) (#76)
  - Added `parametric_autocorrelated_error` as a base R6Class for Parametric Recruitment w/ Autocorrelated Lognormal Errors (#75)
    - Added `berverton_holt_autocorrelated_error` for **Beverton-Holt w/ Autocorrelated Lognormal Error** (Model 10)
    - Added `shepherd_curve_autocorrelated_error` for **Shepherd Curve w/ Autocorrelated Lognormal Error** (Model 11)
    - Added `ricker_curve_autocorrelated_error` for **Ricker Curve w/ Autocorrelated Lognormal Error** (Model 12)
  - Recruitment models **2**, **10**, **11**, **12** added to list to valid recruitment numbers, and can be passed through `set_recruit_model` function.
  - Allow active binding recruitment fields (`recruit_scaling_factor`, `ssb_scaling_factor`, and `max_recruit_obs`) to be settable.
  - Empirical observation tables
    - Added `obs_table` parameter to set observation data at initialization. Default is NULL, generating default values, the original behavior.
    - Added method to set empirical recruitment observation data from data frames
  - Recruitment cli tweaks
    - cli tweaks improve readability with Recruitment Model and Name on Rconsole.
      - `print` now shows `max_recruit_obs` (#71)
    - Rename `print_input_option_name` -> `print_process_error_fields`
    - Minor `read_inp_lines` message tweak to clarify recruitment time period range.
  - Consolidated validate_numeric_substrings code into `read_inp_numeric_lines` function
  - Added option to toggle verbosity when setting new recruitment models via `set_recruit_model`
- Options
  - Consistency cli tweaks between Option classes
  - Renamed options-related files with option prefix and simplified some R6Classes names:
    - renamed R script `max_bounds.R` -> `options_max_bounds.R`
    - renamed R script `user_percentile_summary.R` -> `options_percentile_summary.R`
      - renamed R6Class `user_percentile_summary` -> `percentile_summary`
    - renamed R script `reference_points.R` -> `options_reference_points.R`
    - renamed R script `scaling_factors.R` -> `options_scaling_factors.R`
    - renamed R script `retrospective_adjustments.R` -> `options_retrospective_adjustments.R`
    - renamed R script `output_options.R` -> `options_output.R`
      - renamed R6class `output_options` -> `options_output`
    - renamed R script `optional_options_flags.R` -> `option_flags.R`
      - renamed R6Class `optional_optional_flags` -> `option_flags`
  - "options w/ options_flags" (option_flag class reference for: **max_bounds**, **percentile_summary**, **reference_points**, **scaling_factors**, **retrospective_adjustments**)
    - Fixed an issue where option_flags reference class was improperly referenced, causing unintended consequences using multiple **agepro_model** instances (#85) 
      - Make option_flag R6class a non-shared Reference, and instantiate reference **per agepro_model class instance**. 
      - During **agepro_model** instantiation, each "options w/ options_flags" is instantiated with an option_flag class reference.
    - Use **formals** to detect parameter defaults to determine option_flag is enabled at initialization:
      - **max_bounds**, **percentile_summary**, **reference_points**, **scaling_factors**: Initialize with a non-default parameter: set its flag to TRUE; If all class parameters match default values, set the flag to FALSE.
      - **retrospective_adjustments**: removed default value for `retro_adjust`. If `retrospective_adjust` is missing in a new instances of this class, set `enable_retrospective_adjustments` to FALSE. Otherwise, its TRUE
  - Fixup option_flags inconsistencies, messages
- cli consistency tweaks (_R message_ format) 
  - Reworded Rconsole to represent the keyword parameter class field names.
    - Console alert symbols ℹ️ (or `i`) focuses on keyword parameter classes fields; ◀️ (or `<`) for a interface event (or input line read)
  - Output from initializing **agepro_model** classes or "printing" AGEPRO keyword parameters classes, will print under the _R message_ format. 
    - This resolves an issue with console output changes with [RStudio 2025.05.0+496](https://docs.posit.co/ide/news/#rstudio-2025.05.0). 
    - Pass `print_parameter_table` output through `capture_output_as_message`
      - Replaces use of tibbles for output; Removes **tibble** dependency.
      - Replaces redundant function `cli_print_process_error_table` in process_error class.
  - Added verbosity toggle to specifically show General Parameters to R console during agepro_model initialization. TRUE by default.
- Bootstrap
  - Clarified bootstrap_file warning message for relative path cases
  - cli fix to show Bootstrap File at **agepro_model** initialization
- Replace test AGEPRO input File, Bootstrap File, and json input file with **AGEPRO-GUI** Hawaii Uku Example, based on 2020 Hawaii Uku (_Aprion virescens_) Assessment.
  - Hawaii Uku JSON input file imported from Hawaii Uku AGEPRO input file.
  - Added description file of example.
- Added Vignette as "Article" format (#84)
  - Includes AGEPRO Input File Keyword Parameter Structure and stub for JSON experimental file
- README update (#88)


# ageproR 0.7.3 2025-04-03

- `agepro_inp_model` can be initialized with general agepro model parameters. This change will be consistent with `agepro_model` and `agepro_json_model`. (#64)
  - The default method to generate **agepro_inp_model** remains the same.
  - Fixup/Improve WARNING messages for NULL Recruitment Model Data and Invalid Bootstrap File
- Added `testthat` for basic `agepro_model` unit testing scripts (#48)
  - added **withr** dependency to help with testthat scripts
- Added `model_name` parameter when initializing **case_id**. New agepro_model passes "Unnamed AGEPRO model".
- Updated README
- removed redundant `general_params` parameter checks during agepro_model initialization

# ageproR 0.7.2 2024-10-07

- Use version string (line 1) from AGEPRO Input File to correctly check input file version format against the "current version" input file string (`currentver_inpfile_string`)
- Validation checks for Invalid recruitment model data from NULL or Deprecated Recruitment model 9, when importing and exporting to AGEPRO Input File and JSON Input file (#54)
  - Interrupts export to AGEPRO Input File, if invalid recruitment model data found.
  - JSON input file: Nullify `recruitData`, if invalid recruitment model data found.
- rename `set_projection_analyses -> setup_projection_analyses_value`
  - Function now checks input 'type' parameter value before assigning to projection_analyses_type
- Error message clarifications

# ageproR 0.7.1 2024-09-23

- Revert to `AGEPRO VERSION 4.0` as `currentver_inpfile_string` . Input files will be written to `AGEPRO VERSION 4.0`
  - Supported_inpfile_versions are: `currentver_inpfile_string`, `AGEPRO VERSION 4.0` , `AGEPRO VERSION 4.25`
  - `write_inp(overwrite_as_currentver = FALSE)` save to AGEPRO Input File and keep the supported version of input file (`AGEPRO VERSION 4.25`)
- `write_inp`: Use **withCallingHandlers** to properly handle errors when retrieving AGEPRO Input File formatted Strings from keyword parameter classes (`get_inp_lines`) 
- Rename **agepro_inp_model** private helper function `set_inpfile_version -> write_inpfile_version` 
  - Includes warning if (Supported) AGEPRO Input File Version is not the "current version"
- Updated Roxygen

# ageproR 0.7.0 2024-09-04

- Added AGEPRO **optput_options** (**OPTIONS**) 
  - Added optional options keyword parameters:
    - user_percentile_summary (**PERC**)
    - max_bounds (**BOUNDS**)
    - reference_points (**REFPOINTS**)
    - scaling_factors (**SCALE**)
    - retrospective_adjustments (**RETROADJUST**)
  - Data can be imported/exported to AGEPRO Input File and/or Experimental JSON Input File. 
  - `optional_options_flag`: a shared Reference class for optional options.
    - Contains logical flags to allow user to edit optional options fields: `enable_user_percentile_summary`, `enable_reference points`, `enable_scaling_factors`, `enable_max_bounds`, `enable_retrospective adjustments`.
      - (Re)set to NULL when output options classes is initialized. By default, using initialized default values, set flag to FALSE. Otherwise, set to TRUE. If read from AGEPRO Input File, set to TRUE.     
      - Due to its shared references nature, explicitly collate `optional_options_flags.R` to load before the optional options class R files in DESCRIPTION.
  - Current AGEPRO Input File version is now **`AGEPRO VERSION 4.25`** (`currentver_inpfile_string`): Reflecting changes in **OPTIONS** **StockSummaryFlag** to the new release the AGEPRO calculation engine.
    - AGEPRO input files now written to file under `AGERPRO VERSION 4.25`; Input data imported from `AGEPRO VERSION 4.0` input files will be saved to current version format. 
    - Remove supported AGEPRO Input file version `AGEPRO VERSION 4.2`
- Changed version json list format Experimental JSON Input File:
    - Renamed `version.leagcyVer` -> `version.inpfile_string`: Reflects AGEPRO input file version string
    - Replace `version.ver` with `version.jsonfile_format`: Numeric for JSON Input File format. Marked JSON format to 0 until AGEPRO Keyword Parameters is implemented on JSON Input File.
- Added diagnostic value `ver_rpackage` to return the ageproR version the agepro model was created.
- Toy Example AGEPRO Input File 
  - Changed version of Toy Example AGEPRO Input File to `AGEPRO VERSION 4.0`
  - Added `PERC` and `OUTPUT` data to toy Example Agepro Input File
- Added validation function `validate_logical_parameter` to validate, handle, and convert potential logical values as numerical values to **output_options** active binding fields.
- Fixes:
  - Fixup/Clarify AGEPRO Input file import error messages
  - Fix/updated **rstudioapi** checking character version strings to see if VSCode is currently used when opening save file dialog. Refactor redundant check functions.
  - Added cli prompts when **import_agepro_inp_model** is used
  - Added alternative package install method `pak::pkg_install` example to README.

# ageproR 0.6.2 2024-03-20

- Revise R6 class method order to: public, active, private for maintainability  

# ageproR 0.6.1 2024-03-18

- Rename inplines -> inp_lines for consistency (#37)
- Rename general, bootstrap, process_error, and recruitment json_list active binding functions to `json_list_object`
- process_error (#47)
  - Cleanup proj_years validation code
  - Moved `setup_projection_tables` as a private helper method.
- Resolved `tibble::as_tibble` column name deprecation (#44)
- Updated Roxygen Dependency to 7.3.1

# ageproR 0.6.0 2024-03-11

- Added `mortality_fraction_prior_spawn`
  - Data can be imported/exported to AGEPRO Input File/Experimental JSON Input File
  - added `BIOLOGICAL` data to toy Example Agepro Input File
  - changing `time_varying` creates a new `proportion_total_mortality_matrix` with default values and the number of columns depends on the `time_varying` state. The behavior is similar to AGEPRO-GUI  
  - Explicitly state *natural_mortality* and *fishing_mortailty* to `proportion_total_mortality_matrix` to avoid ambiguity with gender abbreviations
- Improved projection_years validation: multiple assertions (from the checkmate package) can be prompted to Rconsole. 
  - Integrated sequence validation error check wrapped as a custom checkmate assertion. 
- Fixed multi recruitment model number support for agepro_model `set_recruit_model` (#49)
  - Included multiple assertions (from checkmate) to check `set_recruit_model` argument is a numeric vector that matches a list of valid AGEPRO recruitment model numbers.
  - Added custom checkmate assertion wrapper to check `set_recruit_model` argument is empty or has multiple arguments.
- Update DESCRIPTION

# ageproR 0.5.1 2024-01-31

* recruitment 
  - Fixed an issue (#39) where a difference of number of recruitment models using the agepro_model function `set_recruit_model` only changed the Recruit Data collection object structure but did not change recruitment probability nor the `num_rec_models` general_param field.
    - Assert that the count of *recruitment model number(s)* (`model_num`) parameter matches `num_recruit_models` parameter at initialization and when importing recruitment data (`read_inp_lines`) from AGEPRO input files. 
      - Added `num_recruit_models` parameter to recruitment initialization
      - Added Sequence years (`seq_years`) parameter to `read_inp_files` to update number of recruit models
    - `agepro_model$set_recruit_model` : This now initializes a new instance of the recruitment class, using the general_params field *number of recruitment models* (`num_rec_models`) value.
    - Use `purrr::map` to validate input for valid recruitment probabilities within the time projection year horizon for each recruitment model in ***Recruitment Probability*** (`recruit_probability`) active binding setter. Recruitment probabilities can be set after Recruitment 
    - Use `purrr::map` to get (and validate) recruitment model numbers from ***Recruitment Model Data*** (`recruit_data`) input and update the ***Recruitment Model Number Vector*** in the `recruit_data` active binding setter.
  - Setter access to general **recruitment** active binding fields: `recruit_scaling_factor`, `ssb_scaling_factor`, `max_recruit_obs`, `recruit_model_num_list`, is set to private. These values can be set using methods of the **recruitment** class, such as during initialization.
  - Improved Recruitment Model Number Validation using the **agepro_model** function `set_recruit_model` 
  - Code cleanup for consistency and clarity
    - Modularize setups for *Recruitment probability*(`recruit_probability`), *Recruitment Model Number vector* (`recruit_model_num_list`), and the *Recruitment Model Data* (`recruit_data`) into private helper functions. 
    - Renamed field `model_collection_list -> recruit_data` for clarity. 
     - Renamed common **recruit_model** active binding `recruit_data -> json_recruit_data`
    - Replaced `observed_years` to `sequence_projection_years` field.
    - Renamed recruitment's `set_recruit_model` function to `initialize_recruit_model` to reflect its intended functionality and set it as a private helper function.
    - Renamed field `max_rec_obs -> max_recruit_obs`
    - Renamed recruitment function `inplines_recruit` (and `inplines_general` from **general_params**) to `get_inp_lines` (#37)
    - Renamed recruitment parameter `cat_verbose -> enable_cat_print`
* Setter access to **general_params** active binding fields: `yr_begin`, `yr_end`, `age_begin`, `age_end`, `num_pop_sims`, `num_fleets`, `num_rec_models`, `discards_present`, `seed` is set to private. These values can be set using methods of the **general_params** class, such as during initialization. This behavior matches AGEPRO-GUI 
* Simplified projection years validation checks to check for numeric 
* Updated roxygen dependency to version 7.3.0
  - Replaced deprecated doctype package in with  **\_PACKAGE\_** roxygen package documentation (`ageproR.R -> ageproR-package.R`)


# ageproR 0.5.0 2024-01-10

* Added `harvest_scenario`
* Added `projection_analyses`
  - Added `standard_projection`, `pstar_projection`, and `rebuild_projection`
  - `set_projection_analyses_type`
    - added agepro_model value ***projection_analyses_type*** to determine the 
    model's projection analyses type. 
    - Most models, use `standard` (default). Agepro models using a 
    **PStar Projection Analyses** or a **Population Rebuilding Projection **
    **Analyses** will use `pstar` and `rebuild` respectfully.
* agepro_model
  - added support for `harvest_scenario`, including the projection analyses 
  classes: `pstar_projection` and `rebuild_projection`
  - allow NULL values for discard keyword parameters (`discard` and 
  `disc_weight`). 
    - This is allow NULLs for discard keyword parameter class validation
  - agepro_json_model
    - added `inport_agepro_inp_model` to import **agepro_inp_model** data to 
    **agepro_json_model**
    - added initialization method to `agepro_json_model` to resolve 
    "Argument is not a function" warning messages 
    - added `case_id` to JSON experimental input file (#30) 
* general_params
  - Improve general_params values validation and error messages 
    - Check that the lowest value of `yr_end` is 1 higher that `yr_begin` 
    and `age_end` is 1 higher than `age_begin`.
    - Check values of `age_begin` and `discards_present` are 0 or 1.
  - Reordered `pop_num_sims` active binding to match function argument order 
- discard_fraction
  - Fixed initialize typo. 
  - This also resolves a issue where discards_present in `agepro_model` is 
  imported and exported to file as a numeric/double but used as a logical for 
  comparable statements.
- recruitment 
  - cli tweaks to `read_inp_lines`
    - removed recruitment `cli_recruit_rule` for utility function 
    `div_keyword_header` 
- added utility function `create_parameter_table` 
- Function documentation changes
 
 
# ageproR 0.4.1 2023-10-25

* Code refactoring of `general_params`, `case_id`, and `recruitment`
  * general_params
    - Format private members to top of class
    - Actively binded `general_param` fields that was previously only public 
    - Added `keyword_name` and `inp_keyword` active binding
    - Removed `cli_general_rule` for generalized `cli_header_keyword` instead
    - Fix handling of `discard_present` logicals: convert to numeric.
    - Ensure binded fields are numeric during initialization.
    - `read_inp_lines` use `read_inp_numeric_line`
  * case_id
    - Rename `case_id` field to `model_name` to prevent class name-field 
    ambiguity
    - cli tweaks
  * recruitment
    - Added `keyword_name` and `inp_keyword` active binding
    - Removed `cli_recruit_rule` for generalized `cli_header_keyword` instead
    - Actively binded `model_collection_list` and `recruit_model_number_list` 
    to recruitment R6class
* Actively binded `recruit`, `bootstrap`, and `case_id` to `agepro_model` 
R6class
* Renamed `dialog.R` to `file.R`
  - Moved `assert_numeric_substrings` & `read_inp_numeric_line` from `utils.R` 
  to `file.R`
* Renamed `cli_keyword_heading` -> `div_keyword_header`
* Minor code documentation fixes
  

# ageproR 0.4.0 2023-10-04

* Added AGEPRO Keyword parameters for process error for population and 
fishery processes. (#32)
  - `natural_mortality` (`natmort`)
  - `maturity_fraction` (`maturity`)
  - `fishery_selectivity` (`fishery`)
  - `discard_fraction` (`discard`)
  - `jan_stock_weight_age` (`stock_weight`)
  - `spawning_stock_weight_age` (`ssb_weight`)
  - `mean_population_weight_age` (`mean_weight`)
  - `landed_catch_weight_age` (`catch_weight`)
  - `discard_weight_age` (`disc_weight`)
* Implemented AGEPRO input file and experimental JSON input file support for 
process error parameters. (#12),
  - Ensure only "Weight of Age" Process Errors parameters return 
  `input_option` and `time_varying`, if it is a valid weight of age option.
  - Exclude `discards` and `disc_weight` if `agepro_model` doesn't have 
  `discards_present`
  - NOTE: Support for Import process data from auxiliary data file location 
  (`input_option == 1`) is not supported right now.
* Added `[NATMORT]`, `[MATURITY]`, `[FISHERY]`, `[STOCK_WEIGHT]`, 
`[SSB_WEIGHT]`, `[MEAN_WEIGHT]`, and `[CATCH_WEIGHT]` to toy example AGEPRO 
example input data file. Added keyword parameter data is based on AGEPRO GUI's 
example input file _Uku Projection Base_.
* `agepro_model` instantiates Process Error parameters (and `general_params`) as 
active get/set fields
* Added `projection_years`:
  - Handles the ambiguous use of `projection_years`: interpreted as a single 
  integer representing the count of projection years _or_ a vector of sequential 
  values representing a vector of "years".
* `assert_numeric_substrings`: fixed regex numeric string check to include 
negatives.
* `general_params`
  - Renamed `discards` to `discards_present`
  - `seed` defaults to pseudo random number generator
* cli tweaks to print out bootstrap fields and variables (`keyword_name`, 
`inp_keyword`)
* Recruitment
  - Raised default `num_obervations` of **Empirical CDF Recruitment** to `2`
  - Raised default `low_recruits` and `high_recruits` of 
  **Two Stage Empirical Recruitmet w/ SSB** to `2`
* Experimental JSON input file. **Note: This is a developing file format, **
**and it is unsupported with the AGEPRO calculation engine** (#9)
  - If filepath wasn't passed to `write_json`, it will now use the file dialog 
  window. (#11)
  - Added `read_json`.
  - Subset Empirical Recruitment table vector to format as a JSON list in a 
  list object structure
  - Added **Two Stage Empirical Recruitment w/ SSB** JSON list object
  - Fixup formatting of recruit's `type` (recruitment model number) (#29)
* Removed unused `output.R` and `read_input.R`
  - This removes the **rprojroot** and **usethis** dependencies.

# ageproR 0.3.2  2023-08-09

* Fixed `ver_legacy_string` typo that affected the creation of the
`agepro_model` class.

# ageproR 0.3.1  2023-08-03

* Tweaks to R Console cli when loading AGEPRO input file.
  - Improved AGEPRO Input File version validation check.
  - Tweaked Invalid Bootstrap Filename Warning message. 

# ageproR 0.3.0  2023-07-31

Renamed package as ageproR to reflect the package as a supplement to Jon 
Brodziak's original AGEPRO program.

* Export values from AGEPRO Keword parameter exported as a list of character 
strings defined by the _AGEPRO Reference Manual_ input keyword parameter 
data structure (Table 3) and recruitment model structure (Table 4):
 - GENERAL (General Options)
 - CASEID (Case Id)
 - RECRUIT (Recruitment)
 - Recruitment model data:
   - Empirical Recruitment Distribution
   - Empirical Cumulative Distribution Function of Recruitment
   - Two-Stage Empirical Recruits Per Spawning Biomass Distribution
   - Two-Stage Empirical Cumulative Distribution Function of Recruitment
   - Beverton-Holt Curve w/ Lognormal Error
   - Ricker Curve w/ Lognonormal Error
   - Shepherd Curve w/ Lognormal Error
* Implemented method to exported strings (GENERAL, CASEID, RECRUIT & its 
recruit model data, BOOTSTRAP) to AGEPRO input file (*.inp)
* Fixed user cancellation validation check from save file dialog
* Fixed GENERAL parameter order (#22)
* Added LICENSE

# ageproj 0.2.0  2023-06-21

* Added BOOTSTRAP.
  * Added `bootstrap$set_bootstrap_filename` to set bootstrap file from file 
  dialog
* Added functionality to enable a File Dialog Window to load files.
  * Depending on the capabilities of the executed R console, it will use tcltk 
  or rstudioapi library for file dialog windows, or fallback to `file.choose`. 
* cli tweaks

