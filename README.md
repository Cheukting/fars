# FARS Package [![Build Status](https://travis-ci.org/Cheukting/fars.svg?branch=master)](https://travis-ci.org/Cheukting/fars)

FARS package is designed to read, analize and plot data from > "US National Highway Traffic Safety Administration's Fatality Analysis Reporting System"
(http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS))

## Users function

Functions included in this package are:

- fars_read, reading data file
- make_filename, create a file name for the compressed file
- fars_read_years, read files with all the years
- fars_summarize_years, read files with all the years
- fars_map_state, plot location of all accidents for a specific state in a specific year

## Usages

Having the fars datas as ".csv.bz2" files in the working directory. You can read the summary of how may accidents in each month in the specific years by calling the "fars_summarize_years" function. Aslo calling the "fars_map_state" function will plot all the accident location in a specific state in a specific year. Files can also be readed by calling "fars_read_years" or calling "fars_read". See documentation of individual functions for more details and examples.
