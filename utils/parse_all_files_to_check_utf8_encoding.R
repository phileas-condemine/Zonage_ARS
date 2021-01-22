R_files = list.files(".",recursive = T,pattern = ".R$")

file_path = sample(R_files,1)

check_file_encoding = function(file_path){
  file = readLines(file_path,encoding="UTF-8")
  all(validUTF8(file))
}

R_files[!sapply(R_files,check_file_encoding)]
