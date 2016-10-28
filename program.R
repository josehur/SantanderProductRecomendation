library(data.table)
library(dplyr)
library(lubridate)


# --------------------------- Load data --------------------------------------
training_set <- fread(input = "data/train_ver2.csv") 
test_set <- fread(input = "data/test_ver2.csv")


# --------------------------- Clean data -------------------------------------  


# in order to row bind the two data sets I need to..
test_set$indrel_1mes <- as.character(test_set$indrel_1mes) 

full <- dplyr::bind_rows(training_set, test_set) %>% as.data.table()

# cast date fields
full$fecha_dato <- lubridate::ymd(full$fecha_dato)
full[, fecha_alta := lubridate::ymd(fecha_alta)]

summary(full)
