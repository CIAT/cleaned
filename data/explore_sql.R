db_file<-choose.files()

install.packages("RSQLite")
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), db_file)

tables <- dbListTables(conn)
print(tables)

# Retrieve data from a table
data <- dbGetQuery(conn, "SELECT * FROM feeds_to_import")
print(data)
