# "Cleaning Up the Neighborhood: White Influx and Differential Requests for Services" Replication Repo README

This document summarizes the included datasets (uploaded to [Stanford Digital Repository](https://doi.org/10.25740/yf441gg8487)) and code (located in this repository) to replicate and reproduce tables in Dahir et al.'s (2024) article "Cleaning Up the Neighborhood: White Influx and Differential Requests for Services", published in _Socius: Sociological Research for a Dynamic World_. These materials are intended for users to replicate all tables and figures in the article.

## Data

Data are stored in Stanford Digital Repository [here](https://doi.org/10.25740/yf441gg8487).

**df_full.csv**: This dataset includes the analytic sample of block groups, their predicted trash, and number of 311 reports. Also includes neighborhood-level characteristics sourced from the American Community Survey.

**df_map.dbf/.prj/.shp/.shx**: This dataset includes all block groups with corresponding polygons for mapping. 

**df_gent.csv**: This dataset is aggregated to the census tract and contains corresponding gentrification information.

For all datasets, the accompanying codebook (**Cleaning_Up_Codebook.xlsx**) details each column’s values and their meanings.

## Scripts

**tables-figures.R** uses the datasets to generate all of the tables and figures in the paper.
