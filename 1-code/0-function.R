


#' xlsx mtg into mtg file
#'
#' Convert xlsx mtg into mtg file
#'
#' @param xlsx_file Path to the xlsx file
#' @param mtg_file  Path to the mtg file
#'
#' @return
#' @export
#'
#' @examples
xlsx_to_mtg = function(xlsx_file, mtg_file){
  mtg = readxl::read_excel(xlsx_file,sheet = 1,col_names = FALSE)
  data.table::fwrite(mtg, file = mtg_file, sep = "\t",col.names = FALSE)
}


# Apply xlsx_to_mtg to all files in directories
all_xlsx_to_mtg= function(xlsx_dir, mtg_dir){
  xlsx_paths = list.files(xlsx_dir,full.names = TRUE,pattern = "^tree")
  
  mtg_names = basename(xlsx_paths)
  mtg_names = gsub(pattern = "xlsx", replacement = "mtg",mtg_names)
  mtg_paths = paste0(mtg_dir,mtg_names)
  
  for(i in seq_along(xlsx_paths)){
    xlsx_to_mtg(xlsx_file = xlsx_paths[i], mtg_file = mtg_paths[i])
  }
}
