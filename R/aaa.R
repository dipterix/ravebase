#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus do_nothing
#' @importFrom dipsaus shiny_is_running
#' @importFrom dipsaus add_to_session
#' @import raveio
NULL


# --------------------------- Utility functions ------------------------


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    rave_fatal(msg)
  }
}

`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}

deparse1 <- function(expr, collapse = ' '){
  paste(deparse(expr), collapse = collapse)
}

# --------------------------- Dev-use ----------------------------------
soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  rave_warn('Function {call[[1]]} is soft-Deprecated. Call: {deparse1(call)}')
}

hard_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  rave_fatal('Function {call[[1]]} is soft-Deprecated. Call: {deparse1(call)}')
}


# --------------------------------- Misc -------------------------------

# FIXME

save_meta <- raveio:::save_meta
load_meta <- raveio:::load_meta
RAVESubject <- raveio:::RAVESubject
RAVEProject <- raveio:::RAVEProject
RAVEPreprocessSettings <- raveio:::RAVEPreprocessSettings
RAVEEpoch <- raveio:::RAVEEpoch
as_rave_subject <- raveio:::as_rave_subject
as_rave_project <- raveio:::as_rave_project
rand_string <- raveio:::rand_string
# LFP_electrode <- raveio:::LFP_electrode



#' Temporary path where large cached data should be stored
#' @param temp whether use temporary path
#' @param check whether to ensure the folder; if true, then the folder will be
#' created if missing
#' @return Absolute path of cache path.
rave_cache_path <- function(temp = FALSE, check = FALSE){
  if( temp ){
    tdir <- file.path(tempdir(), 'raveio_tempdir')
  } else {
    # always return true value
    tdir <- raveio::raveio_getopt('tensor_temp_path', temp = FALSE)
  }

  re <- file.path(tdir, 'persist_cache')

  if(check){
    # create path, add README
    raveio::dir_create2(re)
    rdm <- file.path(re, 'README.txt')
    if(!file.exists(rdm)){
      writeLines(c(
        'This folder stores cached (loaded) RAVE subject data to speed up loading large datasets. ',
        'Everything is managed by RAVE so please do not edit files. ',
        'However, you may remove this folder if it takes too much space. Please **close all** active RAVE sessions before doing so.'
      ), rdm)
    }
  }


  normalizePath(re, mustWork = FALSE)
}

