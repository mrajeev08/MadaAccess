write_create <- function(obj, path, write_function, ...) {
  
  dir_name <- dirname(path)
  
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }
  
  if(is.character(write_function)) {
    get(write_function)(obj, path, ...)  
  }
  
  if(is.function(write_function)) {
    write_function(obj, path, ...)
  }
  
  if(!is.function(write_function) & !is.character(write_function)) {
    print("Error: arg write_function is not a function or is not loaded!")
  }
}

# a ggsave function with arguments switched
ggsave_it <- function(plot, filename, ...) {
  ggsave(filename, plot, ...)
}

# getting safe paths for here without using comma's
safe_path <- function(path, sep = "/") {
  
  path_list <- unlist(strsplit(path, sep))
  do.call(here::here, as.list(path_list))

}

