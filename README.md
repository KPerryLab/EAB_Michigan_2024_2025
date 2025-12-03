# EAB_Michigan_2024_2025
This project is associated with Aaron Tayal's M.S. thesis, specifically Chapter 1, which is 
titled, "The long-term impacts of emerald ash borer on ash populations in forests near the 
epicenter of invasion"

We visited long-term research plots located in the suburbs of Detroit, MI. We surveyed for the
abundance and health of ash regeneration. We also set up pan traps to monitor the presence
of parasitoid wasps of EAB.

This readme explains most folders in this repository, and their contents.

QGIS_map: this folder contains a map of the study sites which was made using the open-source
mapping software, QGIS.

Raw_data: this folder has all the data as it was orgininally entered into the spreadsheets. 
Data was initially entered into an Excel spreadsheet, and then each page was written as a
separate .csv file, to make importing into R easy. There is one Excel file for 2024, titled
"EAB_Michigan_2024_raw_data.xlsx" and one for 2025, titled "EAB_Michigan_2025_raw_data.xlsx".
Inside the Excel files, the first page is metadata, which explains the variables we recorded.

Cleaned_data: this folder contains spreadsheets where data has been cleaned or summarized
from the raw data. The most important files are: 
-->"ash_by_transect.csv", where each row is a transect we visited, and the columns are counts
and densities of ash regeneration (see my thesis for definitions of seedlings, saplings,
understory (small) trees, and canopy (big) trees)
-->"ash_by_plot.csv", where each row is a plot (there were three plots in each transect)
-->"EAB_Michigan_2024_2025_plot_centers_with_hydro.csv", which has a list of all the plots 
we relocated in 2024-2025, along with the center trees and their diameters. It also has the 
hydrology class listed, which is the column labelled "mstrlvl".

R_scripts: this folder has all the R scripts necessary to clean the data, summarize it, run
statistical analyses, and create figures and tables. The most important scripts are:
-->"2024_2025_ash_occurence.R", which takes the raw .csv files and summarizes ash occurence at
the plot and transect levels. It also takes the individual ash trees and counts them up and 
calculates basal area.
-->"stats_signs_symptoms.R", which runs statistical analyses for the EAB signs and symptoms on
ash trees, in relation to tree diameter.
-->"VERSION2_stats_ash_occurence.R", which runs statistical tests to determine whether the
abundance of ash regeneration depends on soil hydrology class
-->"2025_trees.R". In 2025 we did a survey of 30 hydric plots, and we counted all tree genera, 
not just ash trees. This R script summarizes and cleans this data, generating tables of tree
counts and basal area by genus and by species.

figures: this folder has the figures that I made for my thesis








