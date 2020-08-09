#' @title Load or attach 'RAVE' subject
#' @name rave_load
#' @description Load or attach 'RAVE' a subject to current analysis context.
#' The goal is to allow analysis and visualization to be focused on one that
#' subject.
#' @param subject character, the format is \code{project/subject}, for example
#' \code{"demo/DemoSubject"}
#' @param epoch character, name of epoch, located in subject's 'meta' folder,
#' with file name \code{"epoch_<name>.csv"}
#' @param reference character, name of reference scheme, located in subject's
#' 'meta' folder, with file name \code{"reference_<name>.csv"}
#' @param before_onset seconds before trial onset
#' @param after_onset seconds after trial onset
#' @param electrodes electrodes to load, if missing then default to all
#' possible electrodes
#' @param attach whether to attach loaded repository for analysis
#' @param test whether return \code{NULL} instead of raising errors when
#' no subject is attached
#' @return \code{rave_load} and \code{rave_attach} returns loaded repository,
#' an instance of \code{\link{RAVERepository}} class. \code{attached_repo}
#' returns currently loaded repository. If no repository loaded then it raises
#' errors.
#' @details \code{rave_attached} is a list providing various of methods that
#' handles attached subjects. It also allows 'RAVE' modules designed for
#' single-subject analysis to get information of what's to be analyzed.
#'
#' \code{epoch} and \code{reference} are located in subject's meta folder. The
#' folder path is \code{"<RAVE data_path>/project/subject/rave/meta/"}. Epoch
#' files start with \code{"epoch_"}, and its name is decided by the characters
#' following the prefix. For example, an epoch named "auditory" will have file
#' name \code{"epoch_auditory.csv"}. This rule applies to reference files,
#' except that the prefix is \code{"reference_"}
#'
#' \code{before_onset} and \code{after_onset} affects the amount of data
#' to be loaded. For example, \code{before_onset=1} and \code{after_onset=2}
#' will result in total 3 seconds of data to be loaded: 1 second before trial
#' onset and 2 after onset. The reason to load data before onset is to leave
#' certain time range for baseline. If the trial lasts for 2 seconds, loading
#' 3 seconds of data allows around 1 second to be counted into baseline.
#' The trial onset time for each trial is defined at column \code{"Time"} in
#' the epoch file.
#'
#' If \code{attach=TRUE}, then \code{rave_load} is equivalent to
#' \code{rave_attach}.
#'
#'
NULL

#' @rdname rave_load
#' @export
rave_load <- function(subject, epoch, reference, before_onset, after_onset, electrodes, attach = FALSE){
  rave_repos <- get('rave_repos')

  if(missing(subject)){
    rave_repos[['..current_repo']] <- list()
  }
  repo <-
    loaded_rave_repository(
      subject = subject,
      reference = reference,
      epoch = epoch,
      before_onset = before_onset,
      after_onset = after_onset
    )
  if(missing(electrodes)){
    electrodes <- repo$subject$electrodes
  }
  repo$preload_electrodes <- electrodes

  if(attach){
    rave_repos[['..current_repo']] <- list(
      signature = repo$signature,
      preload_electrodes = repo$preload_electrodes,
      before_onset = before_onset,
      after_onset = after_onset
    )
  }

  repo
}

#' @rdname rave_load
#' @export
rave_attach <- function(subject, epoch, reference, before_onset, after_onset, electrodes){
  call <- match.call()
  call[[1]] <- quote(rave_load)
  call$attach <- TRUE
  eval(call)
}

#' @rdname rave_load
#' @export
attached_repo <- function(test = FALSE){
  # rave_repos[['..current_repo']] <- list(
  #   signature = repo$signature,
  #   preload_electrodes = 13:20
  # )
  rave_repos <- get('rave_repos')
  info <- rave_repos$..current_repo
  if(is.list(info) && length(info$signature) && info$signature %in% names(rave_repos)){
    return(rave_repos[[info$signature]])
  }
  if(!test){
    rave_fatal('Cannot find any subject attached. Please run `rave_attach(...)` or `rave_load(..., attach=TRUE)` to attach RAVE subject first.')
  }
  return()
}


`_rave_get_meta` <- function(name){
  repo <- attached_repo()
  switch(
    name,
    'electrodes' = {
      elec <- repo$subject$meta_data(meta_type = 'electrode')
      refs <- repo$reference_table
      merge(elec, refs, all.x = TRUE, by = 'Electrode')
    }, frequencies = {
      repo$subject$meta_data(meta_type = 'frequencies')
    }, time_points = {
      repo$subject$meta_data(meta_type = 'time_points')
    }, trials = {
      repo$epoch$table
    }
  )
}

`_rave_get_valid_electrodes` <- function(electrodes){
  repo <- attached_repo()
  if(missing(electrodes)){
    electrodes <- repo$subject$electrodes
  }
  electrodes[!electrodes %in% repo$ignored_electrodes]
}


`_rave_get_loaded_electrodes` <- function(){
  attached_repo()
  info <- raveio::get_val2(get('rave_repos'), key = '..current_repo')
  info$preload_electrodes
}

`_rave_get_power` <- function(use_cache = TRUE, .old = FALSE, ...){

  if('referenced' %in% names(list(...))){
    stop('$get_power(referenced=) is no longer supported.')
  }

  repo <- attached_repo()
  info <- raveio::get_val2(get('rave_repos'), key = '..current_repo')

  power <- repo$epoch_continuous_signals(electrodes = info$preload_electrodes, dtype = 'power')
  if(.old){
    power <- raveio::lazyarray_to_tensor(power, drop_partition = TRUE)
    power <- power$subset(Electrode ~ Electrode %in% info$preload_electrodes, drop = FALSE, data_only = FALSE)
  }
  power
}

`_rave_get_phase` <- function(use_cache = TRUE, .old = FALSE, ...){

  if('referenced' %in% names(list(...))){
    stop('$get_phase(referenced=) is no longer supported.')
  }

  repo <- attached_repo()
  info <- raveio::get_val2(get('rave_repos'), key = '..current_repo')

  phase <- repo$epoch_continuous_signals(electrodes = info$preload_electrodes, dtype = 'phase')
  if(.old){
    phase <- raveio::lazyarray_to_tensor(phase, drop_partition = TRUE)
    phase <- phase$subset(Electrode ~ Electrode %in% info$preload_electrodes, drop = FALSE, data_only = FALSE)
  }
  phase
}

`_rave_get_voltage` <- function(use_cache = TRUE, .old = FALSE, ...){

  if('referenced' %in% names(list(...))){
    stop('$get_voltage(referenced=) is no longer supported.')
  }

  repo <- attached_repo()
  info <- raveio::get_val2(get('rave_repos'), key = '..current_repo')

  voltage <- repo$epoch_continuous_signals(electrodes = info$preload_electrodes, dtype = 'voltage')
  if(.old){
    voltage <- raveio::lazyarray_to_tensor(voltage, drop_partition = TRUE)
    voltage <- voltage$subset(Electrode ~ Electrode %in% info$preload_electrodes, drop = FALSE, data_only = FALSE)
  }
  voltage
}

`_rave_get_sample_rate` <- function(original = FALSE){
  repo <- attached_repo()
  if(original){
    warning('get_sample_rate(original=TRUE) might return multiple sample rates')
    unique(repo$subject$raw_sample_rates)
  } else {
    repo$subject$power_sample_rate
  }
}

`_rave_repository` <- function(){
  attached_repo()
}

`_rave_get_preload_info` <- function(){
  repo <- attached_repo()
  re <- dipsaus::fastmap2()
  re$electrodes <- repo$preload_electrodes

  re$epoch_name <- repo$epoch$name
  re$reference_name <- repo$reference_name

  before_onset <- -repo$time_range[1]
  after_onset <- repo$time_range[2]
  srate_power <- repo$subject$power_sample_rate
  re$time_points_power <- seq.int(- ceiling(before_onset * srate_power), ceiling(after_onset * srate_power))
  re$time_points_phase <- re$time_points_power

  re$frequencies <- repo$subject$meta_data('frequencies')$Frequency
  re$condition <- unique(repo$epoch$table$Condition)

  re
}

#' @rdname rave_load
#' @export
rave_attached <- structure(list(), class = c('ravebase_rave_attached', 'ravebase_readonly'))

#' @export
`$.ravebase_rave_attached` <- function(x, name){
  get0(sprintf('_rave_%s', name), ifnotfound = NULL)
}

#' @export
`[[.ravebase_rave_attached` <- `$.ravebase_rave_attached`

#' @export
names.ravebase_rave_attached <- function(x){
  c('get_meta', 'get_valid_electrodes', 'get_loaded_electrodes', 'get_power',
    'get_phase', 'get_voltage', 'get_sample_rate', 'get_preload_info',
    'repository')
}

#' @export
`$<-.ravebase_readonly` <- function(x, name, value){
  stop('Cannot assign values to read-only object')
}

#' @export
`[[<-.ravebase_readonly` <- `$<-.ravebase_readonly`

#' @export
`[.ravebase_readonly` <- function(x, i, ...){
  stop('Cannot subset read-only object')
}

#' @export
print.ravebase_rave_attached <- function(x){
  cat('<rave_attached>              - query data from attached RAVE subject\n')
  cat('  x$get_power()              - obtain epoched & referenced power data\n')
  cat('  x$get_phase()              - obtain epoched & referenced phase angles\n')
  cat('  x$get_voltage()            - obtain epoched & referenced trace data\n')
  cat('  x$get_meta()               - obtain meta table such as `electrodes`, `trials`, `frequencies` etc.\n')
  cat('  x$get_sample_rate()        - get sampling frequency\n')
  cat('  x$get_valid_electrodes()   - get valid electrode numbers\n')
  cat('  x$get_loaded_electrodes()  - get default electrodes to load\n')
  cat('  x$get_preload_info()       - get loader information\n')
  cat('  x$repository()             - returns current repository object (same as `attached_repo()`)\n')
  invisible(x)
}

