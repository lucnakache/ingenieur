
idx_page=55
a_page=document_pages[[idx_page]][[1]]
a_page=a_page[idx_first_line[idx_page]:idx_last_line[idx_page]]

s=FUN_sequence_of_blanks(a_page)

s_metrics=lapply(s,function(m) {
  sequence_of_blanks=m[1,]
  idx_of_blanks=m[2,]
  
  nb_middle=sum(sequence_of_blanks>10 & sequence_of_blanks<17)
  nb_biggy=sum(sequence_of_blanks>17)
  res=c(nb_middle,nb_biggy)
  part="PROBLEM"
  if (all(res==c(1,0))) {part="left"}
  if (all(res==c(0,1))) {part="right"}
  if (all(res==c(1,1)) | all(res==c(2,0))) {part="both"}
  
  solution=idx_of_blanks[which(sequence_of_blanks>10)]+sequence_of_blanks[which(sequence_of_blanks>10)]
  
  
  
  return(list(res,part,solution))
})


FUN_split_string_at_position=function(a_string,split.points){
  res=substring(
    a_string,
    c(1, split.points + 1),
    c(split.points, nchar(a_string))
  )
  return(res)
}


extracted_texts=lapply(seq_along(a_page),function(x){
  split.points=s_metrics[[x]][[3]]-1
  line=a_page[x]
  side=s_metrics[[x]][[2]]
  extracted_text=str_replace(gsub("\\s+", " ", str_trim(FUN_split_string_at_position(line,split.points))), "B", "b")
  extracted_text=extracted_text[which(sapply(extracted_text,nchar)>0)]
  res=list(side,extracted_text)
})

extracted_texts[[10]]

##########################################################################################









# aparté, afficher la page n°X du document pdf
extract_id_page=function(real_document_page) {
  id_page=real_document_page - biography_starting_page+1
  return(id_page)
}

extract_id_page(122)


a_page=document_pages[[extract_id_page(37)]]

compute_statistics_new_chapter=function(a_page){
  p=".*? promotion$"
  lines_to_check=which(grepl(pattern = p,x = a_page[[1]],ignore.case = TRUE))
  p="[[:upper:]\\s-?]+$"
  the_sequence=sequence_of_blanks(str_extract(a_page[[1]][lines_to_check-1], p))
  return(list(lines_to_check,the_sequence))
  
}


compute_statistics_new_chapter(document_pages[[25]])

tt=lapply(document_pages[20:30],function(a_page){
  compute_statistics_new_chapter(a_page)
})

lapply(tt,"[[",2)





pp=lapply(seq_along(document_pages),function(idx) {
  res=str_extract(document_pages[[idx]][[1]],"[[:upper:]\\s-?]+$")
  cat("\rRecherche de patterns dans la page n°",idx,"____________\r")
  return(res)
  res[res=="-"]=NA
  
  p=".*? promotion$"
  
})



tt=pp[extract_id_page(118)]
document_pages[[extract_id_page(118)]]

zz=lapply(tt,function(x){
  x[x=="-"]=NA
  
  nchar=nchar(x)
  
})













document_pages[[extract_id_page(122)]]







