.env <- new.env()
.onUnload <- function(libpath) {
  if (is.loaded("eTTR", PACKAGE = "eTTR")) {
    library.dynam.unload("eTTR", libpath)
  }
}
