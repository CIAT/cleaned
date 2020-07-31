install.packages("rjson")

library(rjson)

getwd()

setwd("D:/Projects/CLEANED Xtra/cleaned-XtRa/data")

CLEANED <- fromJSON(file ="example.json")

CLEANED

json_data_frame <- as.data.frame(CLEANED)

View(json_data_frame)
