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
  
  test_path = "/home/esc/git_repos/fall_18/work/handwriter_webapp/uploads/OGImages/"
  
  #for when i work on desktop, honestly my laptop and desktop are too weak for this anymore.
  #dt_test_path = "/home/ben/git_repos/csafe/handwriter_webapp/uploads/"
  
  letter_plots_path = "/home/esc/git_repos/fall_18/work/handwriter_webapp/uploads/LetterPlots/"
  thinned_path = "/home/esc/git_repos/fall_18/work/handwriter_webapp/uploads/ThinImages/"
  ogthinned_path = "/home/esc/git_repos/fall_18/work/handwriter_webapp/uploads/OGThinImages/"
  #dependencies
  library(handwriter)
  library(ggplot2)
  library(magick)
  library(reshape2)
  library(igraph)
  
  #target paths for plots: og, thinned, ogthinned, individual letters
  img_path = paste0(test_path,img_name)
  lp_dir = paste0(letter_plots_path,img_name)
  
  
  #meat of the work, talks to handwriter
  img_binary = readPNGBinary(img_path)
  img_thinned = thinImage(img_binary)
  img_processed = processHandwriting(img_thinned,dim(img_binary))
  cat('finished processing\n')
  cat('compiling letter features...\n');
  print(img_processed)
  img_features = gatherFeaturesImages(img_processed$letterList)
  cat('saving individual letter plots..\n')
  
  #create directory for individual images
  dir.create(lp_dir)
  lp_dir = paste0(lp_dir,"/")
  print(str(img_features))
  #save supplementary images
  print(img_processed$letterList)
  SaveAllLetterPlots(img_processed$letterList,lp_dir,dim(img_binary),bgTransparent = FALSE)
  SaveSupplementaryPlots(img_thinned,thinned_path,img_name,dim(img_binary))
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


#img_processed$letterList should be arg, seperates features..
gatherFeaturesImages = function(processed_image){
  featureList = list()
  for(i in 1:length(processed_image)){
    cur = processed_image[[i]]$characterFeatures
    #image = processed_image[[i]]$image
    #why does R like implicitly breaking structures
    #mgcur = c(cur,imageSingle = list(image))
    cur = c(cur,letterCode = processed_image[[i]]$letterCode)
    featureList = append(featureList,list(cur))
  }
  return(featureList)
}

#' nicks function i need for reference
#' SaveAllLetterPlots
#'
#' This function returns a plot of a single letter extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting. Requires the \pkg{\link{magick}} package.
#' @param letterList Letter list from processHandwriting function
#' @param filePaths Folder path to save images to
#' @param documentDimensions Dimensions of original document
#' @return Nothing
#' @seealso \pkg{\link{magick}}
#' @export
SaveAllLetterPlots = function(letterList, filePaths, documentDimensions, bgTransparent = TRUE)
{
  if(is.null(letterList[[1]]$image))
    letterList = AddLetterImages(letterList, documentDimensions)
  
  for(i in 1:length(letterList))
  {
    img= magick::image_read(as.raster(letterList[[i]]$image))
    if(bgTransparent)
      img  = magick::image_transparent(img, "white")
    magick::image_write(path = paste0(filePaths, "letter", i, ".png"), img)
  }
}

SaveSupplementaryPlots = function(thinnedImage, filePath, img_name, documentDimensions, bgTransparent = FALSE){
  newThinned = matrix(1, ncol=documentDimensions[2], nrow=documentDimensions[1])
  newThinned[thinnedImage] = 0
  img = magick::image_read(as.raster(newThinned))
  if(bgTransparent)
    img = magick::image_transparent(img,"white")
  magick::image_write(path = paste0(filePath,img_name,"_thinned.png"),img)
}

