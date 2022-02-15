#' Clean Work Space
#'
#' This function allows you to clear the global environment and remove all plots.
#'
#' Version 1.8 2020-10-16
#'
#' @param pattern pattern of objects name to not be removed (default = "DIR")
#' @param restart restart R session (default = F)
#' @export
#' @rawNamespace importFrom("grDevices", "dev.cur", "dev.off")
#' @rawNamespace importFrom("utils", "loadhistory")

cws <- function(pattern = "DIR", restart = F) {
  # Stop the cluster correctly
  # if(exists("cl")) {
  #   stopCluster(cl)
  #   stopImplicitCluster()
  # }

  # Disconnect driver
  # if(exists("drv")) {
  #   ile <- length(dbListConnections(drv))
  #   lapply(dbListConnections(drv), function(x) dbDisconnect(x))
  #   cat(sprintf("%s connection(s) closed.\n", ile))
  # }

  # Remove all objects
  objs <- ls(pos = ".GlobalEnv")
  if(pattern %in% objs) {
    rm(list = objs[-grep(pattern, objs)], pos = ".GlobalEnv")
  } else {
    rm(list = objs, pos = ".GlobalEnv")
  }

  # Turn off plot visualisation
  if(dev.cur() != 1) {
    dev.off()
  }

  # Restart R
  # if(restart) {
  #   # unload all packages
  #   lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  #   invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE, force=TRUE))
  #   # .rs.restartR()
  # }
  # --> better to use the RStudio restart R shortcut

  # Clear terminal
  cat("\014")

  # Clear command history in Rstudio
  unlink(".blank")
  write("", file=".blank")
  loadhistory(".blank")
}
