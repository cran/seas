"getstnname" <-
  function(id) {
    suppressWarnings(data(mscstn))
    name <- mscstn$name[match(id,mscstn$nid)]
    if(length(name) == 0 || is.na(name)) name <- NULL
    name
  }

