setClassUnion("character_list", c("character", "list"))
setClassUnion("character_numeric", c("character", "numeric"))
setClassUnion("logical_list", c("logical", "list"))
setClassUnion("date_character", c("Date", "character"))
setClassUnion("dataframe_list", c("data.frame", "list"))
setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))

