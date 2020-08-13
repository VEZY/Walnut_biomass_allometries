


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





#' Add variables to mtg
#' 
#' Compute meaningful variables to add to the mtg 
#'
#' @param mtg An MTG
#'
#' @return A data.frame with the data
#' @export
#'
compute_data_mtg = function(mtg){
  # Compute the wood density:
  
  if("dry_weight_p1" %in% mtg$features$NAME){
    mutate_mtg(mtg, dry_weight = node$dry_weight_p1,.symbol = "S")
  }else{
    mutate_mtg(mtg, dry_weight = node$dry_weight_p2,.symbol = "S")
  }
  
  mutate_mtg(mtg, density = node$dry_weight/node$volume_bh,
             density_ph = node$dry_weight/node$volume_ph,
             .symbol = "S")
  
  # Topological order:
  topological_order(mtg,ascend = FALSE)
  # We use basipetal topological order (from tip to root) to allow comparisons between branches of 
  # different ages (the last emitted segment will always be of order 1).
  
  # Compute the total length of the axis:
  mutate_mtg(mtg, 
             axis_length = sum(get_descendants_values(attribute = "length", symbol = "S", link = c("/", "<"), recursive = FALSE)),
             .symbol = "A")
  # Associate the axis length to each segment:
  mutate_mtg(mtg, axis_length = get_parent_value(attribute = "axis_length", symbol = "A"), .symbol = "S")
  
  mutate_mtg(mtg, volume = pi*(((node$diameter*10)/2)^2)*node$length, .symbol = "S") # volume of the segment in cm3
  
  # Volume of wood the section bears (all the sub-tree):
  mutate_mtg(mtg, volume_subtree = sum(get_descendants_values(attribute = "volume", symbol = "S",
                                                              self = TRUE)), .symbol = "S")
  
  # segment diameter / axis length:
  mutate_mtg(mtg, d_seg_len_ax_ratio = node$diameter / node$axis_length, .symbol = "S")
  
  data.tree::ToDataFrameTree(mtg$MTG,"ID","density","density_ph","diameter","length","axis_length",
                             "topological_order","d_seg_len_ax_ratio","dry_weight","volume","volume_subtree")
}
