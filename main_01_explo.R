# Clear la work et la console
rm(list=ls())
cat("\014")

# CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** 
# CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** 
# CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** CHARGEMENT DES PARAMETRES ET DES FONCTIONS *** 

# Localisation du script main & pathfile de config
PARAM_main_folder=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
PARAM_config_pathfile=paste0(PARAM_main_folder,"config.R")

# Chargement du fichier config.R
source(PARAM_config_pathfile,encoding = "UTF-8")

# Chargement des fonctions du dossier fun
invisible(lapply(X = PARAM_fun_files ,FUN = function(x)  source(file =x ,encoding = "UTF-8")))

# Importation des librairies
FUN_install_and_load_packages(needed_libraries = PARAM_needed_libraries)



# ON PEUT COMMENCER A TRAVAILLER! *** ON PEUT COMMENCER A TRAVAILLER! *** 
# ON PEUT COMMENCER A TRAVAILLER! *** ON PEUT COMMENCER A TRAVAILLER! *** 
# ON PEUT COMMENCER A TRAVAILLER! *** ON PEUT COMMENCER A TRAVAILLER! *** 
raw_document = FUN_pdf_to_text(PARAM_pathfile,
                               PARAM_biography_starting_page,
                               PARAM_biography_ending_page)
  
length(raw_document)

document_statistics_df=FUN_get_statistics_for_the_document(raw_document)

document_statistics_df=FUN_compute_prediction_idx_last_line(document_statistics_df)



# retrieve start and end idx
idx_last_line=FUN_get_idx_last_line(raw_document,document_statistics_df)
idx_first_line=FUN_get_idx_first_line(raw_document)


# liste contenant une page par item
# cette page étant un vector de sting
# autant d'élément dans ce vecteur qu'il y a de lignes
document_pages=lapply(raw_document,
                      function(a_page){strsplit(x =a_page ,split ="\r\n" ,fixed = TRUE)})


document_pages[[1]]
