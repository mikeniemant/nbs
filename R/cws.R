#' Clean Work Space (cws)
#'
#' This function allows you to clear the global environment and remove all plots
#'
#' Version 1.7 2020-05-22
#'
#' @param pattern pattern of objects name to not be removed (default = "DIR")
#' @param restart restart R session (default = F)
#' @export
#' @examples
#' cws()

cws <- function(pattern = "DIR", restart = F) {
  # Stop the cluster correctly
  if(exists("cl")) {
    stopCluster(cl)
    stopImplicitCluster()
  }

  # Disconnect driver
  if(exists("drv")) {
    ile <- length(dbListConnections(drv))
    lapply(dbListConnections(drv), function(x) dbDisconnect(x))
    cat(sprintf("%s connection(s) closed.\n", ile))
  }

  # Remove all objects
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[-grep(pattern, objs)], pos = ".GlobalEnv")

  # Turn off plot visualisation
  if(dev.cur() != 1) {
    dev.off()
  }

  # Restart R
  if(restart) {
    # unload all pakcages
    lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
    invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))
    .rs.restartR()
  }

  # Clear terminal
  cat("\014")

  # Clear command history in Rstudio
  unlink(".blank")
  write("", file=".blank")
  loadhistory(".blank")
}
