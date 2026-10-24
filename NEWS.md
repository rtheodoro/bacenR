# bacenR 0.5.1

* `get_institutions()`: the CONGLOMERADO file name (used by Bacen after 2022) is now chosen per downloaded year instead of from the whole `years` vector.
    - Stops with a clear error when `out_dir` is not writable; previously write errors were silently ignored and nothing was saved.
    - Reports how many files were downloaded and which year-months were not available.

* `tidy_institutions()`: fixed silent data loss and harmonized columns across years.
    - Reads the correct sheet when a file has more than one (e.g. `201212COOPERATIVAS.xls` had an "SCM" sheet first, so only ~40 of ~1,250 cooperatives were imported; 2011 BANCOS/ADMCONSORCIO have two position sheets).
    - Recognizes the CNPJ column in CONGLOMERADOS files ("CNPJ PARTIC." / "CNPJ PARTICIPANTE"), which previously caused an error.
    - Keeps rows with a missing CNPJ (e.g. conglomerate participants without CNPJ) instead of dropping them.
    - Removes the "FONTE:"/"Fonte:" footer regardless of case and no longer warns about it.
    - CONGLOMERADO and CONGLOMERADOS files are combined into a single CONGLOMERADOS table.
    - Only raw Bacen files (`YYYYMM<INST>.ext`) are read, so the function no longer re-imports its own CSV output when `path_dir` and `out_dir` are the same.
    - Column names that changed over the years are mapped to the most recent header (see `?tidy_institutions`). E-mail and website columns are assigned by content, since Bacen's headers for them are inconsistent.
    - CONGLOMERADOS `data_inicio` is now a `Date` in every year, including files where Excel had swapped day and month (201912, 202112).
    - Empty unnamed columns are dropped.

# bacenR 0.5.0

* `get_institutions()`: fixed ADMCONSORCIO and CONGLOMERADO bugs, and changed params. Now is possible to select years and months individually.

# bacenR 0.4.4

* `tidy_ifdata_reports()`: removed function to import data directly from .csv and .parquet. This will improve the speed to install the package.

# bacenR 0.4.3

* Vignettes texts improved

* Fixed error handle when website is not available at `get_normative_data()`

# bacenR 0.4.0

* Function added:
    - `tidy_ifdata_reports()`

# bacenR 0.3.1

* Function renamed:
    - From`get_ifdata_values()` to `get_ifdata_reports()`

* Fixed bugs in select report types in `get_ifdata_reports()`

# bacenR 0.3.0

* New functions added:
    - `get_ifdata_registry()`: Download institution registry data from Bacen IFdata Cadastro
    - `get_ifdata_values()`: Download data financial information from IFdata of Brazilian Central Bank (Bacen)

# bacenR 0.2.0

* New functions added:
    - `get_institutions()`: Retrieve a list of financial institutions regulated by the Central Bank of Brazil.
    - `tidy_institutions()`: Tidy the institutions data for easier analysis.

* Function were renamed:
    - From `download_normative_data()` to `get_normative_data()`
    - From `download_normative_txt()` to `get_normative_txt()`
    - From `download_balance_sheet()` to `get_balance_sheet()`
    - From `treatment_balance_sheet()` to `tidy_balance_sheet()`

# bacenR 0.1.0

* Initial CRAN submission.

* Functions for accessing Bacen data, such as:
    - `get_normative_data()`: Retrieve normative acts from the Central Bank of Brazil.
    - `get_normative_txt()`: Download normative act texts.
    - `get_balance_sheet()`: Access the Central Bank's balance sheet data.
    - `tidy_balance_sheet()`: Tidy the balance sheet data for easier analysis.
