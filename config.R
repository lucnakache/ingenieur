# CONFIG
PARAM_needed_libraries=c("pdftools","stringr","ggplot2")
PARAM_data_filename="annuaire88.pdf"

# BUILD PARAMETERS
PARAM_fun_folder=paste0(PARAM_main_folder,"fun/")
PARAM_data_folder=paste0(PARAM_main_folder,"data/")
PARAM_pathfile=paste0(PARAM_data_folder,PARAM_data_filename)
PARAM_fun_files = list.files(path =PARAM_fun_folder ,all.files =FALSE ,full.names = TRUE)

# DOCUMENT PARAMETERS
PARAM_biography_starting_page=13
PARAM_biography_ending_page=182

# PATTERNS PARAMETERS
PARAM_COND1=3
PARAM_COND2=0.15
PARAM_COND3=15