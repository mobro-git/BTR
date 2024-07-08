#### This script pulls in the GHGI tables of interest and compiles them into a singe Excel Workbook
#### location of repo and GHGI must be changed for a new user 
#### GHGI tables must be unzipped into the main folder (rather than individual chapter folders)
#### Matt Zwerling 7/3/2024


library(readxl)
library(xlsx)

# Get names of tables of tables of interest from the sheet names from last year's workbook
sheets = excel_sheets("C:/Users/mzwerlin/Github/BTR/data-extra/ghgi/compiled_tables_2023.xlsx")
sheets = sheets[-1]

# Don't know exactly what this does, but it prevents some errors (from Stack overflow)
Sys.setlocale( 'LC_ALL','C' ) 

wb = createWorkbook()

# Add in table definition sheet from last year
df = read_excel("C:/Users/mzwerlin/Github/BTR/data-extra/ghgi/compiled_tables_2023.xlsx", sheet = "Tables")
sheet = createSheet(wb, "Tables")
addDataFrame(df, sheet=sheet, startColumn=1)

# loop through all table names and add them to the workbook
for(x in sheets){
  df = read.csv(paste0("C:/Users/mzwerlin/OneDrive - Environmental Protection Agency (EPA)/Downloads/main-text/Table ",x,".csv"),check.names = FALSE)
  sheet = createSheet(wb, x)
  addDataFrame(df, sheet=sheet, startColumn=1, row.names=FALSE)
}

# Save workbook
saveWorkbook(wb, "C:/Users/mzwerlin/Github/BTR/data-extra/ghgi/compiled_tables_2024.xlsx")
