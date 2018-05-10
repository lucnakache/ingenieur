# install et load les needed librairies
FUN_install_and_load_packages=function(needed_libraries){
  not_yet_installed_libraries=needed_libraries[!needed_libraries %in% rownames(installed.packages())]
  if (length(not_yet_installed_libraries)>0) install.packages(not_yet_installed_libraries)
  lapply(needed_libraries, require, character.only = TRUE)
}

# convertis un numero de page du pdf à un numero de page du document
FUN_pdfnum_to_listnum=function(a_num){a_num - PARAM_biography_starting_page+1}
FUN_listnum_to_pdfnum=function(a_num){a_num + PARAM_biography_starting_page-1}


# convertis un document pdf en liste R
# on ne garde que les pages qui nous interessent
FUN_pdf_to_text=function(pathfile,biography_starting_page,biography_ending_page){
document = pdf_text(pathfile)
document=document[biography_starting_page:biography_ending_page]
cat("Nombre de pages biographies à traiter :",length(document))
return(document)
}



# Renvoie des statistiques pour une page
# Ces statistiques sont utilisées dans le but de
# réorganiser les champs d'une page
FUN_get_statistics_from_page=function(a_page){
  
  a_page=strsplit(x = a_page,split = "\r\n",fixed = TRUE)[[1]]
  a_page=rev(a_page)
  a_page=str_replace_all(string=a_page, pattern=" ", repl="")
  nb_caracteres=nchar(a_page)
  nb_caracteres_alpha=str_count(string =a_page,pattern = "[[:alpha:]]")
  nb_caracteres_non_alpha=nb_caracteres-nb_caracteres_alpha
  p_non_alpha=nb_caracteres_non_alpha/nb_caracteres
  nb_caracteres_alnum=str_count(string =a_page,pattern = "[[:alnum:]]")
  p_non_alnum=(nb_caracteres-nb_caracteres_alnum)/nb_caracteres
  a_page_tokenized=strsplit(a_page,"")
  max_length_sequence=sapply(a_page_tokenized,function(x) {
    sequence_object=rle(x)
    res=max_length_sequence=max(sequence_object$lengths)
    names(res)=NULL
    return(res)
  })
  names(max_length_sequence)=NULL
  return(list(nb_caracteres,p_non_alpha,p_non_alnum,max_length_sequence,seq(1,length(nb_caracteres)),a_page))
  
}


# Renvoie les statistiques pour tout le document dans un dataframe
FUN_get_statistics_for_the_document=function(document){
  document_statistics=lapply(seq_along(document),function(x){
    cat("page n° :",x,"\r")
    res=FUN_get_statistics_from_page(document[x])
    return(res)
  })
  colonnes_depliees=lapply(seq(1,length(document_statistics[[1]])),function(index){
    res=unlist(lapply(X = document_statistics,FUN = "[[",index))
    return(res)
  })
  document_statistics_df=do.call(cbind.data.frame, colonnes_depliees)
  names(document_statistics_df)=c("n","p_non_alpha","p_non_alnum","max_sequence","idx","lignes")
  return(document_statistics_df)
}



FUN_compute_prediction_idx_last_line=function(document_statistics_df){
  document_statistics_df$max_p=apply(X =document_statistics_df[,c("p_non_alpha","p_non_alnum")] ,MARGIN = 1,FUN = max)
  document_statistics_df$cond1=ifelse(document_statistics_df$max_sequence<=PARAM_COND1,TRUE,FALSE)
  document_statistics_df$cond2=ifelse(document_statistics_df$p_non_alnum<PARAM_COND2,TRUE,FALSE)
  document_statistics_df$cond3=ifelse(document_statistics_df$n>=PARAM_COND3,TRUE,FALSE)
  document_statistics_df$candidat=document_statistics_df$cond1*document_statistics_df$cond2*document_statistics_df$cond3
  return(document_statistics_df)
}



FUN_get_idx_last_line=function(document,document_statistics_df){
  nb_lignes_par_page=sapply(document,function(x){
    length(strsplit(x = x,split = "\r\n",fixed = TRUE)[[1]])
  })
  names(nb_lignes_par_page)=NULL
  document_statistics_df$idx_page=rep(seq(1,length(nb_lignes_par_page)),nb_lignes_par_page)
  list_of_pages=split(document_statistics_df,document_statistics_df$idx_page)
  
  idx_last_line=sapply(list_of_pages,function(x) {
    first_position=min(which(as.logical(x$candidat)))
    n_lignes=nrow(x)
    first_position=n_lignes-first_position+1
    return(first_position)
  })
  
  return(idx_last_line)
  
}



FUN_get_idx_first_line=function(document){
  list_of_pages=lapply(document,function(x){
    list_of_pages=strsplit(x = x,split = "\r\n",fixed = TRUE)[[1]]
    return(list_of_pages)
  })
  
  idx_first_line=sapply(list_of_pages,function(x){
    p=".*? promotion$"
    res=grepl(pattern = p,x = x ,ignore.case =TRUE )
    if(length(which(res))==0) {
      starting_line=999} else {
        starting_line=max(which(res)) + 1}
    return(starting_line)
  })
  
  return(idx_first_line)
}






# FUNCTION : sequence_of_blanks (better version)
# Input : vecteur de strings
# Output : séquence de blanks consécutifs pour chacune des chaînes de caractères
FUN_sequence_of_blanks=function(a_page){
  a=lapply(str_extract_all(a_page," +"),nchar)
  b=lapply(str_locate_all(a_page," +"),function(m) m[,1])
  res=Map(rbind, a, b)
  return(res)
}
