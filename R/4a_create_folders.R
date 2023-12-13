create_folders <- function(paths) {
  lapply(paths,
         function(x) if(!dir.exists(x))
           dir.create(x))
}

