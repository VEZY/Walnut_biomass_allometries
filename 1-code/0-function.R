


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
  
  if("dry_weight_p1" %in% attr(mtg,"features")$NAME){
    mutate_mtg(mtg, dry_weight = node$dry_weight_p1,.symbol = "S")
  }else{
    mutate_mtg(mtg, dry_weight = node$dry_weight_p2,.symbol = "S")
  }
  
  mutate_mtg(mtg, density = node$dry_weight/node$volume_bh,
             density_ph = node$dry_weight/node$volume_ph,
             density_ph_wood = node$dry_weight_wood/node$volume_phse,
             density_ph_bark = node$dry_weight_bark/(node$volume_ph-node$volume_phse),
             .symbol = "S")
  
  # ======== total pathlength of sub-tree):
  mutate_mtg(mtg, pathlength_subtree = sum(descendants(attribute = "length", symbol = "S",
                                                                  self = TRUE)), .symbol = "S")
  # xxxxxxxxxxxxxxxxxxxxxxx
  mutate_mtg(mtg, number_leaves = length(leaves(attribute = "topological_order", symbol = "S")), .symbol = "S")
  
  # density of wood without bark taking account for wood dry weight without bark
  # (Note: without considering the vume of bark)
  # mutate_mtg(mtg, density_wood_only = node$dry_weight_wood/node$volume_bh,
  #            .symbol = "S")
  # 
  # mutate_mtg(mtg, density_wood_on_tot = node$density_wood_only/node$density,
  #            .symbol = "S")
  
  
  
  # add relative change of volume after rehydrating
  mutate_mtg(mtg, volume_delta = (node$volume_ph-node$volume_bh)/node$volume_bh,
             .symbol = "S")

  
  # Topological order:
  topological_order(mtg,ascend = FALSE)
  # We use basipetal topological order (from tip to base) to allow comparisons between branches of 
  # different ages (the last emitted segment will always be of order 1).
  
  # Compute the index of each segment on the axis in a basipetal way (from tip to base)
  mutate_mtg(mtg, 
             segment_index_on_axis = length(descendants(attribute = ".symbol", symbol = "S",
                                                        link = c("/", "<"), 
                                                        continue = FALSE))+1,
             .symbol = "S")
  
  # Compute the total length of the axis:
  mutate_mtg(mtg, 
             axis_length = sum(decompose(attribute = "length", symbol = "S")),
             .symbol = "A")
  # Associate the axis length to each segment:
  mutate_mtg(mtg, axis_length = parent(attribute = "axis_length", symbol = "A"), .symbol = "S")
  
  mutate_mtg(mtg, volume = pi*(((node$diameter/10)/2)^2)*node$length, .symbol = "S") # volume of the segment in cm3
  
  
  # added JD
  mutate_mtg(mtg, cross_section = pi*(((node$diameter/10)/2)^2), .symbol = "S") # area of segment cross section  in cm2
  mutate_mtg(mtg, cross_sec_children = sum(children(attribute = "cross_section", symbol = "S")), .symbol = "S")
  
  # Cross section of the terminal nodes for each node
  mutate_mtg(mtg, cross_sec_leaves = sum(leaves(attribute = "cross_section", symbol = "S"),na.rm = TRUE), 
             .symbol = "S")
  # TODO 
  # somme des sections des UC terminales; a plotter vs section du porteur
  # Puis tester avec une valeur unique pour toutes les UC
  
  
  # Volume of wood the section bears (all the sub-tree):
  mutate_mtg(mtg, volume_subtree = sum(descendants(attribute = "volume", symbol = "S",
                                                   self = TRUE)), .symbol = "S")
  
  # segment diameter / axis length:
  mutate_mtg(mtg, d_seg_len_ax_ratio = node$diameter / node$axis_length, .symbol = "S")
  
  
  # ratio weight bark/wood
  mutate_mtg(mtg, ratio_bark_wood = node$dry_weight_bark / node$dry_weight, .symbol = "S")
  
  
  # data.tree::ToDataFrameTree(mtg$MTG,"ID","density","density_ph","diameter","length","axis_length",
  #                            "topological_order","segment_index_on_axis","dry_weight",
  #                            "volume","volume_subtree")
  data.tree::ToDataFrameTree(mtg,"ID","density","density_ph","volume_ph","volume_phse","volume_delta","diameter","length","axis_length",
                             "topological_order","segment_index_on_axis","dry_weight","dry_weight_bark","ratio_bark_wood",
                             "volume","volume_subtree","cross_section","cross_sec_children",
                             "number_leaves","pathlength_subtree")
}
