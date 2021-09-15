library(stringr)

# path = "global.R"
# path = "server.R"
# path = "ui.R"

unravel_sources = function(path){
  code = readLines(path)
  sources_loc = grep("source\\(",code)
  # print(code[sources_loc])
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
    sources_loc = grep("( |^)source\\(",code)
    code[sources_loc]
    has_source = (length(sources_loc)>0)
    i=i+1
  }
  code 
}

global = unravel_sources("global.R")
server = unravel_sources("server.R")
ui = unravel_sources("ui.R")

code = c(global,"ui<-",ui,"server<-",server,"shiny::shinyApp(ui,server)")

writeLines(code,"app.R")

shiny::shinyAppFile("app.R")


f = "app.R"
script = readLines(f,encoding = "UTF-8")

inputs = stringr::str_extract_all(script,"input\\$[^, )=\\]]*")
inputs = unlist(inputs)
inputs = sort(unique(inputs))
inputs


file.remove("app.R")
