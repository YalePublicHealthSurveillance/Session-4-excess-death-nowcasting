# Using ExcessILI's data archiving functions, returns the most recent copy of
# output obtained by running a function or formula \code{f}, unless this 
# copy doesn't exist or is older (by modification time) than \code{maxage}.
# In that case, \code{f} is run and the output is archived into the folder
# Data/'storeName' as an RDS file, using the function ExcessILI::storeRDS.
#
# @param storeName A string. The name of the folder to store output in
# @param f A function or formula taking no arguments. Formulas are coerced to
#   functions.
# @param maxage How old can any existing archived file be before \code{f} is 
#   called again?


#' ExcessILI/R/fileCache.R
#' @marcusrussi
#' marcusrussi mostRecentTimestamp now returns NA if directory doesn't exist
#' Latest commit 3eddff1 on Apr 13, 2020
#'  History
#'  1 contributor
#' 200 lines (174 sloc)  6.64 KB
#'    
#'    
runIfExpired <- function(storeName, f, maxage=hours(999999999999999999999999999999999999)) {
  basepath <- "Data/"
  mostRecent <- mostRecentTimestamp(storeName, basepath=basepath)
  f <- rlang::as_function(f)
  
  runAndArchive <- function() {
    data <- f()
    storeRDS(data, storeName, basepath)
    data
  }
  
  if (is.na(mostRecent)) 
    return(runAndArchive())
  
  if (mostRecent %--% now() < maxage)
    return(retrieveRDS(storeName, basepath))
  
  runAndArchive()
}




now <- lubridate::now()


retrievePath <- function(fname, basepath='.', goalDate=lubridate::now()) {
  
  # Construct the path to the folder where all copies of 'fname' should be 
  # stored
  fullpath <- file.path(basepath, fname)
  
  # Make sure that this path is a directory I.e., archive/fname.txt needs to 
  # be a folder, not regular file.
  if (!dir.exists(fullpath))
    stop(sprintf("Path %s doesn't exist, or isn't a directory", fullpath))
  
  # Be sure that 'goalDate' is a Date object
  assertthat::assert_that(any("POSIXct" %in% class(goalDate)))
  
  # Get all the files in this directory
  dirListing <- list.files(fullpath)
  
  # If there were no files in the directory, we can't retrieve them!
  if (length(dirListing) == 0)
    stop(sprintf("No files were found in dirctory %s", fullpath))
  
  # Get a list of modification times for each file in the directory
  fullPaths <- file.path(fullpath, dirListing)
  mtimes <- purrr::lift_dl(file.mtime)(fullPaths)
  
  # The interval elapsed between the modified time of each file, and the 
  # goalDate. Our goal is to get the file with the smallest delta.
  absDeltas <-
    purrr::map_dbl(mtimes, ~lubridate::int_length(. %--% goalDate)) %>%
    abs
  
  # Find the index, and return it
  idx <- which(absDeltas == min(absDeltas))[1]
  
  fullPaths[idx]
}


retrieveRDS <- function(fname, basepath='.', goalDate=Sys.time())
  retrievePath(fname, basepath, goalDate) %>% readRDS


storeRDS <- function(obj, fname, basepath='.') {
  
  if (!dir.exists(basepath))
    stop(sprintf("Basepath '%s' does not exist. Cannot write file.", basepath))
  
  fullPath <- file.path(basepath, fname)
  
  # Create the directory for 'fname' if it doesn't exist. Notify the user.
  if (!dir.exists(fullPath)) {
    message(sprintf("Creating directory %s", fullPath))
    success <- dir.create(fullPath, recursive = FALSE, showWarnings = TRUE)
    
    if (any(!success))
      stop(sprintf("Failed to create directory %s", fullPath))
  }
  
  name <- as.character.POSIXt(Sys.time(), format="%Y_%m_%d_%H_%M.rds")
  writepath <- file.path(basepath, fname, name)
  
  saveRDS(obj, writepath)
  
  message(sprintf("Wrote object to %s", writepath))
}


mostRecentTimestamp <- function(fname, basepath='.') {
  
  # Construct the path to the folder where all copies of 'fname' should be 
  # stored
  fullpath <- file.path(basepath, fname)
  
  # Make sure that this path is a directory I.e., archive/fname.txt needs to 
  # be a folder, not regular file.
  if (!dir.exists(fullpath))
    return(NA)
  
  # Get all the files in this directory
  dirListing <- list.files(fullpath)
  
  # If there were no files in the directory, we can't retrieve them!
  if (length(dirListing) == 0)
    return(NA)
  
  # Get a list of modification times for each file in the directory
  fullPaths <- file.path(fullpath, dirListing)
  mtimes <- purrr::lift_dl(file.mtime)(fullPaths)
  
  max(mtimes)
}