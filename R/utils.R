async_work <- function(X, FUN, ..., .globals, .name = 'Untitled', .rs = FALSE, .wait = TRUE, chunk_size = Inf){
  stopifnot2(!length(.globals) || (
      is.list(.globals) && length(names(.globals)) == length(.globals) &&
        (!'' %in% names(.globals))
    ) ,
    msg = '.globals must be a named list'
  )

  if(chunk_size < Inf && !.wait){
    rave_fatal('async_work .want=FALSE, then chunk_size must be `Inf`')
  }

  n <- length(X)

  if(!n){
    return(list())
  }

  nworkers <- rave_options('max_worker')
  nworkers <- min(nworkers, n)
  # partition X
  chunk_size <- max(min(chunk_size, ceiling(n/nworkers)), 1)
  njobs <- ceiling(n / chunk_size)
  mat <- c(seq_along(X), rep(NA, njobs * chunk_size - n))
  dim(mat) <- c(njobs, chunk_size)

  progress <- dipsaus::progress2(.name, max = njobs * .wait + 2, shiny_auto_close = TRUE, quiet = !.wait)

  progress$inc('Process data...')

  tempdir(check = TRUE)
  rds <- tempfile()
  saveRDS(list(
    globals = .globals,
    args = list(...),
    fun = FUN
  ), file = rds)

  progress$inc(sprintf('Finished [0/%s]', njobs))

  res <- dipsaus::fastmap2()
  fs <- dipsaus::fastmap2()
  finished <- rep(0, njobs)
  collect_res <- function(nw = 0){
    not_finished <- finished == 1
    while(sum(not_finished) && sum(not_finished) >= nw){
      Sys.sleep(0.1)
      for(ii in which(not_finished)){
        code <- fs[[ii]]()
        if(code < 0){
          finished[[ii]] <<- 2
          code <- paste(attr(x, 'rs_exec_error'), collapse = '\n')
          rave_fatal(code)
        }
        if(code == 0){
          finished[[ii]] <<- 2
          if(.wait){
            progress$inc(sprintf('Finished [%s/%s]', sum(finished == 2), njobs))
          }
          code <- attr(code, 'rs_exec_result')
          idx <- mat[ii,]
          idx <- idx[!is.na(idx)]
          if(is.list(code) && length(code) == length(idx)){
            for(jj in seq_along(idx)){
              res[[idx[[jj]]]] <- code[[jj]]
            }
          }
        }
      }
      not_finished <- finished == 1
    }
  }


  lapply(seq_len(njobs), function(ii){

    if(sum(finished == 1) >= nworkers){
      collect_res(nworkers)
    }
    name <- sprintf('%s - part %d', .name, ii)
    idx <- mat[ii,]
    idx <- idx[!is.na(idx)]
    el <- X[idx]
    quo <- rlang::quo({
      raveio::raveio_setopt('max_worker', 1L, .save = FALSE)
      with(readRDS(!!rds), {
        if(!length(globals)){
          globals <- list()
        }
        return(do.call('lapply', c(
          list(
            !!el,
            dipsaus::mask_function2(fun, .list = globals)
          ),
          args
        )))
      })
    })
    expr <- rlang::quo_squash(quo)
    fs[[ii]] <- dipsaus::rs_exec(expr, name = name, quoted = TRUE, wait = FALSE, rs = .rs)
    finished[[ii]] <<- 1
  })


  if(.wait){
    collect_res(0)
    return(res)
  } else {
    return(invisible(collect_res))
  }
}


# stopifnot2 <- raveio:::stopifnot2
# rave_options <- ravebase:::rave_options
# res <- async_work(1:100, function(x){
#   Sys.sleep(0.1)
#   x + 1
# }, .globals = NULL, .wait = TRUE, chunk_size = 6)
