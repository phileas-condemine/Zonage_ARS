library(data.table)
library(stringr)
f = "utils/import_file.R"
script = readLines(f,encoding = "UTF-8")

inputs = stringr::str_extract_all(script,"input\\$[^, )=\\]]*")
inputs = unlist(inputs)
inputs = sort(unique(inputs))
