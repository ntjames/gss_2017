com_plt.RData contains complete average annual expenditure data for annual trends plots

dict.RData contains data dictionary variables and codes

maj.RData contains all major interview and diary datasets


data sets that (a) are generated during a preprocessing step and (b) don't need to be regenerated every single time you analyze your data. You can use the `cache()` function to store data to this directory automatically. Any data set found in both the `cache` and `data` directories will be drawn from `cache` instead of `data` based on ProjectTemplate's priority rules.
