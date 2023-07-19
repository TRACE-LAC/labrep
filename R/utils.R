row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {  
    names(data) <- as.character(unlist(data[row_num,]))
    data[-row_num,]
  }
}

generate_age_categories <- function(data) {
  if (length(which(names(data) == "grupo_edad")) <= 0) {
    data <- cbind(data, grupo_edad = NA)
    data[, ncol(data)] <- sapply(data$edad, define_age_categorie)
  }
  return(data)
}

define_age_categorie <- function(age) {
  #config_path <- system.file("extdata", "config.yml", package = "sivirep")
  config_path <- "C:/Users/geral/Documents/TRACE/tmp/sivirep/inst/extdata/config.yml"
  categorie_conditionals <- config::get(file = config_path, "age_categorie_conditionals")
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  age_values <- unlist(strsplit(age, " ", fixed = T))
  categorie <- categorie_labels[1]
  
  if ("AÑOS" %in% age_values) {
    age_num <- as.numeric(age_values[1])
    i <- 1;
    for (conditional in categorie_conditionals) {
      if (eval(parse(text = conditional)) == T) {
        categorie <- categorie_labels[i]
      } 
      i <- i + 1
    }
  }
  
  return(categorie)
}