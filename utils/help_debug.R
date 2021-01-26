# pour trouver dans quel script il y a un pb

files <- c(paste0("utils/",list.files("utils/")), "server.R", "ui.R", "global.R")
files <- grep("\\.R", files, value = T)

string_to_look_for = 'pop plm'

for(i in 1:length(files)){
  print(files[i])
  for(j in 1:length(readLines(files[[i]]))){
    if(files[i] != "utils/help_debug.R" & length(grep(string_to_look_for, readLines(files[[i]])[j])) > 0){
      print("here !")
    }
  }
}

