LazyBar <- R6::R6Class("LazyBar", public = list(
  n = NULL,
  i = 0,
  time = NULL,
  initialize = function(n){
    self$n <- n
    self$time <- numeric(n+1)
    self$start()
  },

  start = function(){
    self$i <- 0
    self$time[[1]] <- proc.time()[[3]]
    invisible(self)
  },

  tick = function(){
    if(self$i >= self$n) stop("Process ended")
    self$i <- self$i + 1
    self$time[[self$i+1]] <- proc.time()[[3]]
    self
  },

  print = function(){
    if(!interactive()) return(invisible(self))
    now <- proc.time()[[3]]
    left <- self$n-self$i

    dtime <- diff(self$time[seq(1, self$i+1)])
    eta <- sum(((dtime[[self$i]]-dtime[[1]])/ifelse((te <- self$i-1) ==0, 1, te))*seq(1, left)) + left*dtime[[self$i]]
    if(eta<0){
      eta <- mean(dtime)*left
    }

    width <- getOption("width")- nchar("|100.0% ~elapsed: 99 h 00 m 00 s") - 2

    bar <- paste0(c(
      "|",
      paste(rep.int("=", floor(self$i/self$n * width))),
      paste(rep.int("-", width- floor(self$i/self$n * width))),
      "|",
      format(round(self$i/self$n *100, 1), width = 4),
      "% ",
      "~",
      if(self$i<self$n){
        c("eta: ", print_time(eta))
      } else {
        c("elapsed: ", print_time(self$time[[self$n]] - self$time[[1]]))
      }
    ),
    collapse = "")
    blank <- max(c(0, getOption("width") - nchar(bar, "width")))
    cat("\r", bar, rep.int(" ", blank), sep = "")
    if(self$i==self$n) cat("\n")
    utils::flush.console()
    invisible(self)
  }


)
)

#' @export
lazyProgressBar <- function(n){
  LazyBar$new(n)
}

print_time <- function(x) {
  if (x < 60) {
    paste(round(x), "s")
  } else if (x < 60 * 60) {
    paste(floor(x / 60), "m", round(x %% 60), "s" )
  } else {
    paste(floor(x / (60 * 60)), "h",
          floor(x %% (60*60)/60), "m",
          round(x %% (60 * 60) %% 60), "s")
  }
}
