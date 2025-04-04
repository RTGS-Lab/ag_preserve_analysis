Agricultural Preservation Program Analysis

This repository contains code and data processing scripts for analyzing the impacts of agricultural preservation programs across selected cities in Hennepin County. The analysis focuses on evaluating program enrollment and property tax/value outcomes using assessor data and program participation records. The workflow is structured to ensure clean, replicable data pipelines and reproducible research outputs.

Data Sources

	•	All data sources can be found on the shared drive in Google under 202501_003_Easements_Hockert --> Data.
	•	Annual Assessor Data (2005–2022): Parcel-level property and tax assessment data (Conservation Easment Data V4 Main All Years.txt)
	•	Ken’s Linked Text Files: Program enrollment, city LNTC tax rates, and parcel sales (Conservation Easment Data V3 Program Linkage.txt,Conservation Easment Data V3 City Rate Data.txt, Conservation Easment Data V3 Sales.txt)
	•	CPI Deflator: Used to convert nominal values into real dollars (cpi.xlsx)
	•	Hennepin County City files: Match city ID to names and for spatial matching (Hennepin Cities(Report).csv, bdry_census2010counties_ctus.gpkg)
	•	Parcel Geometry (optional): For spatial joins and shapefile outputs (created in script)

Key Scripts

Assessor_data.R

	•	Reads and combines yearly .xlsx and .txt files with consistent column formats
	•	Merges program participation data (PROGRAM) with the main parcel dataset
	•	Standardizes program codes into binary indicators:
		•	G_Green_Acres
		•	R_Rural_Preserve 
		•	P_Platted_Land 
		•	O_Open_Space
  
	•	Calculates tax burden variables, including:
		•	CITY_LNTC_TAX = LOCAL_NET_TAX_CAPACITY × CITY_LNTC_TAX_RATE
		•	REAL_* values using CPI-adjusted conversions

program_analysis_subset_cities.R

	•	Filters to parcels with and without program participation
	•	Calculates city-level parcel counts, program shares, and average tax burdens
	•	Produces county-level and city-level summaries of:
		•	Tax share
		•	Real and nominal city taxes
		•	Parcel and acreage counts by program type

ag_preserve_visualization.R
  
	•	Implements facet-wrapped visualizations of program effects across:
		•	Years
		•	Super property types (residential, commercial, etc.)
		•	Program types (AG_PRESERVE, G_Green_Acres, R_Rural_Preserve)

ag_preserve_models.R
  
	•	Performs OLS regressions to estimate the relationship between:
		•	Share of land in a program and average tax burden
		•	Program enrollment and changes in parcel-level tax payments
 
Output Highlights

	•	Aggregated tax impact visuals at the parcel, city, and county levels
	•	Tables of program vs. non-program parcel distributions
	•	Log-level and percentage change plots of key metrics (tax share, assessed value)

Usage

Run the scripts in order:

	1.	Assessor_data.R: Prepares, cleans, and merges data
 
	2.	Main.R: loads additional data and preps data from assessors_data.R
 
 	3.	program_analysis_subset_cities.R: Subsets and analyzes program effects

  	4. 	ag_preserve_visualization.R: if interested but not required.

   	5. 	ag_preserve_models.R: produces the models specific to the ag preservation programs.
 

Plots and model summaries are saved or printed inline.

Notes

	•	Subrecord-level PIDs are handled with PID_SUBRECORD
	•	Cities with high-density development (e.g., Minneapolis, Bloomington) are excluded from some analyses
	•	Time trends and property-type interactions are included in linear models to control for heterogeneity
