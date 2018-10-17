library(tidyverse)

dict <- readxl::read_excel("data-raw/CIDACS_100M_Cohort_Data_Dictionary.xlsx")

length(unique(dict$name_en))
length(dict$name_en)
dict$name_en[duplicated(dict$name_en)]

# are all variable names unique within database?
dict %>%
  group_by(db_name_en) %>%
  summarise(n = n(), nu = length(unique(name_en)))
# db_en                        n    nu
# <chr>                    <int> <int>
# Bolsa Família (BF)          16    16
# Original CIDACS variable     2     2
# SIM                         68    66
# SINASC                      48    48
# Single Registry (CADU)      65    65
# NA                          10    10

# add db column
dict$db <- dict$db_name
dict$db[dict$db == "Variável original Cidacs"] <- "CIDACS"
dict$db[dict$db == "Bolsa Família (BF)"] <- "BF"
dict$db[dict$db == "Cadastro Único (CADU)"] <- "CADU"
table(dict$db)

# there are some duplicates within SIM
dict_sim <- filter(dict, db == "SIM")
dnames <- dict_sim[duplicated(dict_sim$name_en), ]$name_en
data.frame(filter(dict_sim, name_en == dnames[1]))
data.frame(filter(dict_sim, name_en == dnames[2]))

table(dict$presence_en)
table(dict$presence)
table(dict$type)

# check periods at end of labels
dict$label_en[grepl("\\.$", dict$label_en)]
which(grepl("\\.$", dict$label_en)) + 1

## fix the dictionary values
##---------------------------------------------------------

# remove dates from map
idx <- which(dict$map_en == "dd mm yyyy")
dict$map_en[idx] <- NA
dict$map[idx] <- NA

# get index of variables that are mapped
idx_en <- grepl("^[0-9]+ - ", dict$map_en) & !is.na(dict$map_en)
idx <- grepl("^[0-9]+ - ", dict$map) & !is.na(dict$map_en)
all(idx == idx_en)
dict$map_en[idx]

# look at what's left in mapping column that isn't NA
idx2_en <- !idx & !is.na(dict$map_en)
idx2 <- !idx & !is.na(dict$map)
all(idx2 == idx2_en)
dict$map_en[idx2]
dict$name[idx2]
# these don't need to be mapped
# TODO: add a check to seriescmae when reading in to recode 0 (Null) and 99 (Inconsistency)
dict$map_en[idx2] <- NA
dict$map[idx2] <- NA

dict$type <- tolower(dict$type)
dict$type[which(idx)] <- "factor"
dict$type[dict$type == "byte"] <- "integer"
dict$type[dict$type == "long"] <- "double"
dict$type[dict$type == "string"] <- "character"

dict$type[idx2]

table(dict$type, useNA = "always")

# add type to variables that don't have it
tidx <- which(is.na(dict$type))
dict$name_en[tidx]
dict$label_en[tidx]

dict$type[tidx[grepl("_date$", dict$name_en[tidx])]] <- "date"
dict$type[tidx[grepl("_code$", dict$name_en[tidx])]] <- "integer"

table(dict$type, useNA = "always")

to_map <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (grepl("^[0-9]+ - ", x)) {
    x <- gsub("\r", "", x)
    x2 <- strsplit(x, "\n")[[1]]
    code <- gsub("^([0-9]+).*", "\\1", x2)
    x3 <- gsub("^[0-9]+ \\- (.*)", "\\1", x2)
    names(x3) <- code
    # put "Null" at the end
    idx <- unname(which(x3 == "Null"))
    if (length(idx) == 1)
      x3 <- x3[c(setdiff(seq_along(x3), idx), idx)]
    return(as.list(x3))
  } else {
    return(NA)
  }
}

dict <- dict %>%
  mutate(
    map_en_orig = map_en,
    map_orig = map,
    map_en = map(map_en_orig, to_map),
    map = map(map_orig, to_map)
  )

select(dict, name, map, map_en)

snsc_dict <- filter(dict, db == "SINASC")
sim_dict <- filter(dict, db == "SIM")

cdcs_dict <- select(dict, -questions)
use_data(cdcs_dict, overwrite = TRUE)
