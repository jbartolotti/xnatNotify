
#https://stackoverflow.com/questions/9122600/r-command-line-file-dialog-similar-to-file-choose

#' Text-based interactive file selection.
#'@param root the root directory to explore
#'             (default current working directory)
#'@param multiple boolean specifying whether to allow
#'                 multiple files to be selected
#'@return character vector of selected files.
#'@examples
#'fileList <- my.file.browse()
UTILS.my.file.browse <- function (root=getwd(), multiple=F) {
  # .. and list.files(root)
  x <- c( dirname(normalizePath(root)), list.files(root,full.names=T) )
  isdir <- file.info(x)$isdir
  obj <- sort(isdir,index.return=T,decreasing=T)
  isdir <- obj$x
  x <- x[obj$ix]
  lbls <- sprintf('%s%s',basename(x),ifelse(isdir,'/',''))
  lbls[1] <- sprintf('../ (%s)', basename(x[1]))

  files <- c()
  sel = -1
  while ( TRUE ) {
    sel <- menu(lbls,title=sprintf('Select file(s) (0 to quit) in folder %s:',root))
    if (sel == 0 )
      break
    if (isdir[sel]) {
      # directory, browse further
      files <- c(files, my.file.browse( x[sel], multiple ))
      break
    } else {
      # file, add to list
      files <- c(files,x[sel])
      if ( !multiple )
        break
      # remove selected file from choices
      lbls <- lbls[-sel]
      x <- x[-sel]
      isdir <- isdir[-sel]
    }
  }
  return(files)
}


UTILS.getFile <- function(file){
  if(is.na(file)){
      sinfo <- Sys.info()
    if(sinfo[['sysname']] == 'Linux'){
      file <- UTILS.my.file.browse()
    } else {
      file <- file.choose()
    }
  }
  myfile_path <- paste(strsplit(file,'\\',fixed=TRUE)[[1]],collapse='/')
  spl <- strsplit(myfile_path,'/')[[1]]
  myfile_name <- spl[[length(spl)]]
  myfile_dir <- paste(spl[1:(length(spl)-1)],collapse = '/')

  myfile <- list(path = myfile_path, filename = myfile_name, dir = myfile_dir)
  return(myfile)
}




