".First.lib" <- function(lib, pkg) {
  library.dynam("seas", pkg, lib)
  setSeasOpts()
}
".Last.lib" <- function(libpath){
  library.dynam.unload("seas",libpath)
}
