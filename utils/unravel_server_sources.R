
path = "server.R"
code = readLines(path)
sources_loc = grep("source\\(",code)
has_source = (length(sources_loc)>0)
i=1
while(has_source){
  one_loc = sources_loc[1]
  code_to_source = code[one_loc]
  # print(code_to_source)# check raw value if error, maybe there is a paste0 or something weird in the source()
  #get file name
  file_name = stringr::str_extract(code_to_source,"(source\\()('|\")([^'\"]*)\"")
  file_name = gsub("^source\\(","",file_name)
  file_name = gsub("'","",file_name)
  file_name = gsub("\"","",file_name)
  print(paste(i,file_name))
  # get code in the file sourced
  sub_code = readLines(file_name)
  begin = paste0("##### ",file_name," #####")
  sub_code = c(begin,sub_code,"##########")
  # append codes
  code = c(code[1:(one_loc-1)],sub_code,code[(one_loc+1):length(code)])
  # check for sources calls
  sources_loc = grep("source\\(",code)
  has_source = (length(sources_loc)>0)
  i=i+1
}

code = c(readLines("global.R"),"ui<-",readLines("ui.R"),"server<-",code,"shiny::shinyApp(ui,server)")
  
writeLines(code,"app.R")

shiny::shinyAppFile("app.R")

file.remove("app.R")
