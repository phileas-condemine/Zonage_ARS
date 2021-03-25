# options(error=recover) 
test_res = testthat::test_file('tests/shinytest/manual_test_dl.R')
test_res = invisible(print(test_res))
has_error = sum(test_res$error)>0
if(has_error)stop("Error with manual_test_dl.R")

test_res = testthat::test_file('tests/shinytest/interaction_w_tableau_zonage.R')#remember the ps/reg/agr are picked randomly thus it could be useful to run this several times !
test_res = invisible(print(test_res))
has_error = sum(test_res$error)>0
if(has_error)stop("Error with interaction_w_tableau_zonage.R")

test_res = testthat::test_file('tests/shinytest/download_map_zonage_arrete.R')
test_res = invisible(print(test_res))
has_error = sum(test_res$error)>0
if(has_error)stop("Error with download_map_zonage_arrete.R")