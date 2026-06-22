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
