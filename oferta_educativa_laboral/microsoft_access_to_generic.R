######
#
#
#
# Oct 2024
######


######
getwd()
setwd('/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/data/data_UP/access_SIAP_18092024/processed/')
dir()
######


######
library(odbc)
library(DBI)

odbc::odbcListDrivers()
######

######
# Specify the path dynamically:
driver_n <- "Driver={MDBToolsODBC};"
# "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
# "Driver={MDBToolsODBC};"

file_n <- 'Qna_17_SIAP_2024_copy.accdb'

dbq_n <- paste0("DBQ=", file_n, ";")
# DBQ=/path/to/your/database.accdb
# DBQ=/path/to/your.mdb"

# Create a connection string:
conn_string <- paste0(driver_n, dbq_n)
conn_string
######


######
# Connect to the database (direct path or DSN-based):
conn <- dbConnect(odbc::odbc(),
                  .connection_string = conn_string
                  )

# List tables
dbListTables(conn)

# Read a table
data <- dbReadTable(conn, "YourTableName")

# Close connection
dbDisconnect(conn)
######



######
library(RODBC)
# Connect to the Access database:
conn <- odbcDriverConnect(conn_string)

# Connect to the Access database
# conn <- odbcConnect(file_n, uid = "", pwd = "")


# Retrieve data
data <- sqlFetch(conn, "YourTableName")
close(conn)

# List tables available in the database
tables <- sqlTables(conn)
print(tables)

# Retrieve data from a specific table
data <- sqlFetch(conn, "YourTableName")
print(head(data))

# Close the connection
odbcClose(conn)
######
