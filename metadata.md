# Metadata: Agricultural Preservation Program Analysis

This metadata file describes the primary datasets used in the agricultural preservation program analysis in Hennepin County.

---

## `data/Conservation Easment Data V4 Main All Years.txt`

**Description:**  
Annual parcel-level assessor data from 2005–2022, including estimated market values, tax capacity, program enrollment flags, and property classifications.

**Key Columns:**
- `PID`: Parcel ID
- `TAX_YEAR`: Tax assessment year
- `SUBRECORD_NO`: Sub-parcel identifier
- `PROPERTY_TYPE`: Raw assessor property classification
- `CITY_ID`, `SCHOOL_ID`, `WATERSHED_ID`, `CITY_STORM_SEWER_ID`: Jurisdictional codes
- `EMV_LAND`, `EMV_BLDG`: Estimated market value of land and building
- `ESTIMATED_MARKET_VALUE`: Combined market value
- `LOCAL_NET_TAX_CAPACITY`: Net taxable capacity for the parcel
- `LOCAL_NET_TAX_CAPACITY_RATE`: Corresponding local tax rate
- `LNTC_CITY_SHARE`: City share of local tax capacity
- `AG_PRESERVE`, `HOMESTEAD`: Program flags (1 = enrolled)
- `ACREAGE`: Parcel land area
- `PROGRAM`: Program type (G, R, P, O)
- `HIGHEMV_LAND`, `HIGHEMV_BLDG`: Highest market value observed per parcel
- `CTU_NAME`: City-township name
- `year`: Year (redundant with `TAX_YEAR`)
- `super_property_type`: Consolidated category (e.g., residential, commercial)
- Binary columns for property types (e.g., `residential`, `multi_family_residential`, `agricultural_farm`, etc.)

---

## `data/Conservation Easment Data V3 Program Linkage.txt`

**Description:**  
Program enrollment linkage data used to merge historical program participation with assessor data.

**Key Columns:**
- `TAX_YEAR`: Year of enrollment
- `PID`, `SUBRECORD_NO`: Parcel and subrecord identifiers
- `PROGRAM`: Type of enrolled preservation program
- `PROPERTY_TYPE`: Raw property code
- `HIGHEMV_LAND`, `HIGHEMV_BLDG`: Historical high market value for land and buildings

---

## `data/Conservation Easment Data V3 Sales.txt`

**Description:**  
Parcel sales data, used optionally for understanding property transaction timing and value changes.

**Note:** Column names are currently unlabeled (`V1`–`V4`) and may require manual identification.

---

## `data/Conservation Easment Data V3 City Rate Data.txt`

**Description:**  
City-level LNTC tax rates used to compute city tax burdens.

**Key Columns:**
- `year`: Year of tax rate
- `CITY_ID`: City identifier
- `CITY_LNTC_TAX_RATE`: City share of local net tax capacity rate

---

## `Hennepin Cities(Report).csv`

**Description:**  
Lookup file mapping `CITY_ID` to city names for readable outputs and joins.

**Key Columns:**
- `CITY_ID`: Unique numeric city identifier
- `NAME`: Name of the city

---

## `gpkg_bdry_census2010counties_ctus`

**Description:**  
Geospatial file (GPKG) providing city/township boundaries for spatial matching and mapping.

**Key Columns:**
- `COCTU_ID`, `COCTU_CODE`, `COCTU_DESC`: County-township unit identifiers and descriptions
- `CO_CODE`, `CO_NAME`: County codes and names
- `CTU_ID`, `CTU_CODE`, `CTU_NAME`: City/township identifiers
- `geom`: Geometry (polygon)

---

## `cpi.xlsx`

**Description:**  
Annual CPI index used for deflating nominal values to real dollars for tax and value trends.

**Key Columns:**
- `Year`: Calendar year
- `Annual`: CPI index value

---

## Notes on Data Usage

- `PID` and `SUBRECORD_NO` are used in combination to uniquely identify sub-parcel records.
- Time trend adjustments rely on `TAX_YEAR` and CPI deflation.
- City-level aggregation is performed using `CITY_ID` and `CTU_NAME`.
- Program participation is encoded in the `PROGRAM` column and standardized into binary columns for modeling.

---

## Related Scripts

- `Assessor_data.R`: Cleans and merges core datasets
- `program_analysis_subset_cities.R`: Subsets and analyzes program participation effects
- `ag_preserve_visualization.R`: Produces plots across years, property types, and programs
- `ag_preserve_models.R`: Runs statistical models evaluating tax burden and program relationships

---

_Last updated: 2025-04-01_