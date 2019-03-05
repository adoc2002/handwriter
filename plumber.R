# plumber.R

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message issss: '", msg, "'"))
}

#' monolithic test function intended to return json of feature output..
#' step 1: image name with predefined url path to process
#' step 2: add functionality to send the image itself (without any need for its name)
#' step 3: allow both, improved ui options
#' 
#' also a reference on how to use the package
#' @param img_name img to be processed
#' @get /plumbFeatures
plumbt = function(img_name, debug = FALSE){
  #dependencies, todo: add conditional to check if first time server init
  test_path = "/home/esc/git_repos/fall_18/work/handwriter_webapp/uploads/"
  dt_test_path = "/home/ben/git_repos/csafe/handwriter_webapp/uploads/"
  library(handwriter)
  library(ggplot2)
  #library(reticulate)
  library(reshape2)
  library(igraph)
  img_path = paste0(test_path,img_name)
  #setwd(test_path)
  img_binary = readPNGBinary(img_path)
  img_thinned = thinImage(img_binary)
  img_processed = processHandwriting(img_thinned,dim(img_binary))
  cat('finished processing\n')
  cat('update to character features.. enumerating now...\n');
  #rename to character
  img_features = extract_character_features(img_processed$graphemeList,dim(img_binary))
  cat('processing has been finished, should be trying to push to node server')
  list(img_features)
}
#    R List Appendage
#    lm_rm_nodelist = list()
#    for(i in 1:length(character)){
#      cur = character[[i]]$nodesInGraph
#      lm_rm_nodelist = append(lm_rm_nodelist,list(list(leftMost = cur[[1]],rightMost = cur[[length(cur)]])))
#    }
#    return(lm_rm_nodelist)
#  }

gatherFeatures = function(processed_image){
  featureList = list()
  for(i in 1:length(processed_image$letterList)){
    cur = processed_image$letterList[[i]]$characterFeatures
    featureList = append(featureList,list(cur))
  }
  return(featureList)
}

