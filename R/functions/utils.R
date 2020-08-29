write_create <- function(obj, path, write_function) {
  
  dir_name <- dirname(path)
  
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }
  
  if(is.character(write_function)) {
    get(write_function)(obj, path)  
  }
  
  if(is.function(write_function)) {
    write_function(obj, path)
  }
  
  if(!is.function(write_function) & !is.character(write_function)) {
    print("Error: arg write_function is not a function or gettable function!")
  }
}

