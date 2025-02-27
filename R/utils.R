#' @title Convertir una fila a un encabezado
#' @keywords internal
row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {
    names(data) <- as.character(unlist(data[row_num, ]))
    data[-row_num, ]
  }
}

#' @title Generar las categorias de edad
#' @export
generate_age_categories <- function(dataset) {
  if (!any(names(dataset) == "grupo_edad")) {
    data_ages <- cbind(dataset, grupo_edad = NA)
    data_ages[, ncol(data_ages)] <- sapply(data_ages$edad,
                                           define_age_category)
    return(data_ages)
  } else {
    return(dataset)
  }
}

#' @title Definir las categorias de edad
#' @keywords internal
define_age_category <- function(age) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_conditionals <- config::get(file = config_path,
                                       "age_categories")$conditionals
  category_labels <- config::get(file = config_path,
                                 "age_categories")$categories
  age_values <- unlist(strsplit(age, " ", fixed = TRUE))
  category <- category_labels[1]
  print(age)
  if ("años" %in% age_values) {
    i <- 1
    for (conditional in category_conditionals) {
      if (eval(parse(text = conditional))) {
        category <- category_labels[i]
      }
      i <- i + 1
    }
  }
  return(category)
}

#' @title Agregar caracter
#' @keywords internal
add_character <- function(value, char) {
  init_pos <- regexpr("años|año|mes|meses|día|días|dias", value)[1]
  if (init_pos > -1) {
    value <- paste0(substring(value, 1, init_pos - 1),
                    char,
                    substring(value, init_pos))
  }
  return(value)
}

#' @title Obtener el top de virus por grupo de edad
#' @export
get_top_virues_age <- function(dataset,
                               col_age = "grupo_edad",
                               age_groups = c("< 2 AÑOS",
                                              "2 A 4 AÑOS")) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <- config::get(file = config_path,
                                 "age_categories")$labels
  categories <- config::get(file = config_path,
                            "age_categories")$categories
  categories <- epitrix::clean_labels(categories)
  viruses <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_age) %in% age_groups) %>%
    dplyr::arrange(!!dplyr::sym(col_age), "casos")
  viruses[[col_age]] <- epitrix::clean_labels(viruses[[col_age]])
  top_viruses <- viruses %>%
    dplyr::group_by(!!dplyr::sym(col_age)) %>%
    dplyr::top_n(3, wt = .data$casos) %>%
    dplyr::mutate(!!dplyr::sym(paste0(col_age, "_etiqueta")) :=
                    category_labels[match(!!dplyr::sym(col_age),
                                          categories)]) %>%
    dplyr::ungroup()
  top_viruses_ordered <- data.frame()
  text <- ""
  for (age in categories) {
    age_group_rows <- which(top_viruses[[col_age]] == age)
    if (length(age_group_rows) > 0) {
      age_group_values <-
        top_viruses[which(top_viruses[[col_age]] == age), ]
      age_group_values <- age_group_values %>%
        dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
      for (i in seq_len(nrow(age_group_values))) {
        age_group <- age_group_values[i, ]
        if (i < nrow(age_group_values)) {
          text <- paste0(text, " ",
                         age_group$grupo_edad_etiqueta,
                         " se presentan casos de ",
                         age_group$etiqueta,
                         " (",
                         age_group$porcentaje,
                         "%), ")
        } else {
          text <- paste0(text, "y ",
                         age_group$grupo_edad_etiqueta,
                         " se presentan casos de ",
                         age_group$etiqueta,
                         " (",
                         age_group$porcentaje,
                         "%).")
        }
      }
    }
  }
  return(top_viruses_ordered)
}

#' @title Obtener el grupo de edad con mayor número de casos
#' @keywords internal
get_max_age_group <- function(dataset, col_name) {
  age_max <- -1
  age <-
    stringr::str_split(dataset[[col_name]],
                       stringr::fixed("_"))[[1]]
  if (length(age) <= 3) {
    age_max <- as.numeric(age[1])
  } else {
    age_max <- as.numeric(age[3])
    age_max <- age_max + 1
  }
  return(age_max)
}

#' @title Obtener el porcentaje por grupo de edad
#' @export
get_perc_viruses_age <- function(dataset,
                                 col_name = "grupo_edad") {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1:2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym(col_name)))
  perc_group_4 <-
    round((sum(vs_age_group_4$casos) * 100) / sum(dataset$casos), 1)
  return(perc_group_4)
}

#' @title Obtener el texto del consolidado de los virus por grupo de edad
#' @export
get_cons_viruses_age_text <- function(dataset,
                                      col_name = "grupo_edad",
                                      text_group) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  text_cons_virues <- NULL
  vs_age_group_2 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_2 <- vs_age_group_2[1:3, ]
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_4 <- vs_age_group_4[1:3, ]
  text_cons_virues <-
    paste0(text_cons_virues,
           get_text_viruses(dataset = vs_age_group_2,
                            tam = nrow(vs_age_group_2)),
           text_group,
           get_text_viruses(dataset = vs_age_group_4,
                            tam = nrow(vs_age_group_4)))
  return(text_cons_virues)
}

#' @title Obtener el texto de tosferina por grupo de edad
#' @export
get_tosferina_text_sex <- function(dataset, figure) {
  percentage_female <-
    dataset[which(dataset$genero == "Femenino"), ]$porcentaje
  percentage_male <-
    dataset[which(dataset$genero == "Masculino"), ]$porcentaje
  sex_major <- c("femenino", percentage_female)
  sex_less <- c("masculino", percentage_male)
  if (isTRUE(percentage_female < percentage_male)) {
    sex_major <- c("masculino", percentage_male)
    sex_less <- c("femenino", percentage_female)
  }
  text_sex <- paste0(
    "Con respecto al género, ",
    "se observa que el ", sex_major[2], "%", " corresponden al género ",
    sex_major[1], " y el ", sex_less[2], "%",
    " al género ", sex_less[1], "(figura ", figure,
    ")."
  )
  text_values <- list(
    text = text_sex, major = sex_major,
    less = sex_less
  )
  return(text_values)
}

#' @title Obtener el texto de la proporción acumulada con Sars CoV 2
#' @export
get_prop_text <- function(dataset) {
  top_viruses <- dataset %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  top_viruses <- top_viruses[1:3, ]
  text_viruses <- get_text_viruses(dataset = top_viruses,
                                   tam = nrow(top_viruses))
  return(text_viruses)
}

#' @title Obtener el texto de los virus por grupo de edad
#' @export
get_text_viruses <- function(dataset, tam) {
  text_viruses <- NULL
  for (i in seq(1:tam)) {
    virus <- dataset[i, ]
    if (i < tam) {
      text_viruses <- paste0(text_viruses, virus$etiqueta,
                             " (", virus$porcentaje, " %), ")
    } else {
      token <- " y "
      if (startsWith(virus$etiqueta, prefix = "I")) {
        token <- " e "
      }
      text_viruses <- paste0(substr(text_viruses,
                                    1,
                                    nchar(text_viruses) - 2), token,
                             virus$etiqueta, " (", virus$porcentaje, " %)")
    }
  }
  return(text_viruses)
}

#' @title Completar las categorías de edad
#' @export
complete_age_categories <- function(data_grouped,
                                    event_name,
                                    event_label) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  categorie_labels <- config::get(file = config_path,
                                  "age_categories")$age_categories
  for (label in categorie_labels) {
    if (!any(data_grouped == label) || is.na(any(data_grouped == label))) {
      new_row <- data.frame(grupo_edad = label,
                            casos = 0,
                            porcentaje = 0,
                            evento = event_name,
                            etiqueta = event_label)
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  return(data_grouped)
}

#' @title Obtener los valores de los subtipos de un virus
#' @export
get_subtypes_values <- function(viruses, subtypes) {
  subtypes_values <- NULL
  for (virus in viruses) {
    if (virus$name %in% subtypes) {
      other_vrs <- virus$other_viruses
      subtypes_values <- c(subtypes_values, other_vrs$values)
    }
  }
  return(subtypes_values)
}

#' @title Obtener los subtipos de la Influenza
#' @export
get_influenza_viruses <- function(viruses, events) {
  influeza_viruses <- viruses
  remove_index <- NULL
  for (i in seq(1, length(viruses))) {
    virus <- viruses[[i]]
    if (!(virus$name %in% events)) {
      remove_index <- c(remove_index, i)
    }
  }
  influeza_viruses <- influeza_viruses[-remove_index]
  return(influeza_viruses)
}

#' @title Obtener la tabla de casos por semana epidemiológica de Tosferina
#' @export
get_table_epiweek_tosferina <- function(report_data, epiweek) {
  table_data <- data.frame(SE = report_data$semana_epidemiologica,
                           positivos = report_data$porcentaje)
  table_data$SE <- as.numeric(table_data$SE)
  table_data <- table_data %>%
    dplyr::arrange(.data$SE <= as.numeric(epiweek))
  table_data <- table_data %>%
    dplyr::arrange(.data$SE)
  return(table_data)
}

#' @title Añadir las semanas epidemiológicas faltantes
#' @export
add_missing_weeks <- function(dataset, col_epiweek) {
  max_epiweek <-
    max(as.numeric(dataset[[col_epiweek]]))
  if (max_epiweek < 53) {
    diff_epiweek <- 53 - max_epiweek
    dataset_aux <- data.frame()
    dataset_aux <- rbind(dataset_aux,
                         data.frame(semana =
                                  seq(max_epiweek + 1, 53),
                                casos = rep(0, diff_epiweek),
                                total_casos = rep(0, diff_epiweek),
                                porcentaje = rep(0, diff_epiweek)))
    names(dataset_aux)[names(dataset_aux)
                       == "semana"] <- col_epiweek
    dataset <- rbind(dataset, dataset_aux)
  }
  return(dataset)
}

#' @title Convertir grupos de edad a columnas
#' @export
convert_age_groups_as_cols <- function(dataset) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  category_labels <- c("etiqueta", category_labels)
  data_groups <- dataset %>%
    dplyr::select(.data$etiqueta, .data$grupo_edad, .data$casos) %>% # Seleccionar columnas relevantes
    tidyr::pivot_wider(
      names_from = .data$grupo_edad, # Columna que se convierte en encabezados
      values_from = .data$casos      # Valores que llenan la tabla
    )
  cols_order <- factor(colnames(data_groups),
                       levels = category_labels)
  data_groups <- data_groups %>%
    dplyr::select(dplyr::all_of(levels(cols_order)))
  return(data_groups)
}








#############################################
### CODIGO AGREGADO POR WILLIAM : GITHUB API###########
#### TODAS LAS FUNCIONES ##############################

#' Generar ruta de datos históricos
#'
#' Esta función construye la ruta completa donde se encuentran los datos históricos dentro del paquete especificado.
#'
#' @param paquete Nombre del paquete donde se buscan los datos. Por defecto es `"labrep"`.
#' @param libreria Nombre de la carpeta dentro de `extdata` que contiene los datos. Por defecto es `"data-historica-periodoepi"`.
#'
#' @return Ruta completa del directorio donde están los datos históricos.
#' @export
#'
#' @examples
#' ruta <- generar_ruta_datos_historicos()
#' print(ruta)
generar_ruta_datos_historicos <- function(paquete = "labrep", libreria = "data-historica-periodoepi") {
  # Validate that 'paquete' is installed and available
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Error: El paquete '", paquete, "' no está instalado o no es accesible.")
  }
  # Get the package's extdata directory path
  pkg_extdata_path <- base::system.file("extdata", package = paquete)
  # Check if the package has an extdata directory
  if (pkg_extdata_path == "") {
    stop("Error: No se encontró el directorio 'extdata' en el paquete '", paquete, "'.")
  }
  # Construct the final path
  historical_data_path <- base::file.path(pkg_extdata_path, libreria)
  
  return(historical_data_path)
}




#' Validar dominio de correo
#'
#' Esta función verifica si un correo electrónico pertenece al dominio `@saludcapital.gov.co`.
#'
#' @param correo Dirección de correo electrónico a validar.
#'
#' @return Un valor lógico (`TRUE` si el correo es válido, `FALSE` si no lo es).
#' @export
#'
#' @examples
#' validar_dominio("usuario@saludcapital.gov.co") # TRUE
#' validar_dominio("otro@dominio.com") # FALSE
#' 

validar_dominio <- function(correo) {
  # Validate that the input is a non-empty character string
  if (!is.character(correo) || length(correo) != 1 || !nzchar(correo)) {
    stop("Error: El correo debe ser un string no vacío.")
  }
  # Define regex pattern for the domain validation
  patron <- "^[a-zA-Z0-9._%+-]+@saludcapital\\.gov\\.co$"
  # Return TRUE if it matches, FALSE otherwise
  return(base::grepl(patron, correo))
}



#' Obtener usuario desde un correo
#'
#' Esta función extrae el nombre de usuario de una dirección de correo electrónico.
#'
#' @param correo Dirección de correo electrónico de la cual se extraerá el usuario.
#'
#' @return Una cadena de texto con el usuario antes del '@'.
#' @export
#'
#' @examples
#' obtener_usuario("usuario@saludcapital.gov.co") # "usuario"
#' obtener_usuario("ejemplo@dominio.com") # "ejemplo"
obtener_usuario <- function(correo) {
  # Validate that the input is a non-empty character string
  if (!is.character(correo) || length(correo) != 1 || !nzchar(correo)) {
    stop("Error: El correo debe ser un string no vacío.")
  }
  # Ensure the email contains '@'
  if (!grepl("@", correo, fixed = TRUE)) {
    stop("Error: El formato del correo no es válido.")
  }
  # Split email and return the first part
  usuario <- base::strsplit(correo, "@")[[1]][1]
  return(usuario)
}



#' Obtener fecha y hora actual
#'
#' Esta función devuelve la fecha y opcionalmente la hora actual en un formato específico.
#'
#' @param hora Valor lógico (`TRUE` por defecto) que indica si se incluye la hora en el formato de salida.
#'
#' @return Una cadena de texto con la fecha en formato `"YYYY_MM_DD"` o `"YYYY_MM_DD_HH_MM"` si `hora = TRUE`.
#' @export
#'
#' @examples
#' obtener_fecha_hora() # "2025_02_25_14_30" (ejemplo con hora)
#' obtener_fecha_hora(FALSE) # "2025_02_25" (ejemplo sin hora)
obtener_fecha_hora <- function(hora = TRUE) {
  # Get the current date and time
  tiempo_actual <- base::Sys.time()
  
  # Format the output based on the 'hora' parameter
  formato <- if (hora) "%Y_%m_%d_%H_%M" else "%Y_%m_%d"
  
  return(base::format(tiempo_actual, formato))
}


#' Construir nombre de archivo
#'
#' Esta función genera un nombre de archivo basado en la fecha, usuario, año y período epidemiológico.
#'
#' @param fecha Cadena de texto con la fecha en formato `"YYYY_MM_DD"`.
#' @param usuario Nombre del usuario asociado al archivo.
#' @param anio_actual Año actual en formato numérico o cadena de texto.
#' @param periodo Número entero que representa el período epidemiológico.
#'
#' @return Una cadena de texto con el nombre del archivo en el formato `"YYYY_MM_DD_usuario_periodo_epi_YYYY_pX"`.
#' @export
#'
#' @examples
#' construir_nombre_archivo("2025_02_25", "usuario", 2025, 12) 
#' # "2025_02_25_usuario_periodo_epi_2025_p12"
construir_nombre_archivo <- function(fecha, usuario, anio_actual, periodo) {
  # Ensure all inputs are character strings
  fecha <- as.character(fecha)
  usuario <- as.character(usuario)
  anio_actual <- as.character(anio_actual)
  periodo <- as.character(periodo)
  
  # Construct the filename
  nombre_archivo <- base::paste0(fecha, "_", usuario, "_periodo_epi_", anio_actual, "_p", periodo)
  
  return(nombre_archivo)
}


#' Crear ruta local de archivo
#'
#' Esta función construye la ruta completa de un archivo en el sistema de archivos local.
#'
#' @param base_path Ruta base donde se almacenará el archivo.
#' @param nombre_archivo Nombre del archivo que se agregará a la ruta base.
#'
#' @return Una cadena de texto con la ruta completa del archivo.
#' @export
#'
#' @examples
#' crear_path_local("data/historica", "archivo.rds")
#' # "data/historica/archivo.rds"
crear_path_local <- function(base_path, nombre_archivo) {
  return(base::file.path(base_path, nombre_archivo))
}


#' Guardar un DataFrame en el computador 'local' en formato RDS
#'
#' Esta función guarda un DataFrame en un archivo RDS con compresión para optimizar el almacenamiento,
#' en la carpeta del paquete 'labrep' en el computador local (no en remoto, en el repositorio de github).
#'
#' @param dataframe DataFrame a guardar en formato RDS.
#' @param path Ruta completa donde se almacenará el archivo RDS en la máquina local.
#'
#' @return Un valor lógico: `TRUE` si el archivo se guardó correctamente, `FALSE` si ocurrió un error.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5])
#' guardar_datos_local(df, "datos.rds")
#' # Devuelve TRUE si se guardó con éxito
guardar_datos_local <- function(dataframe, path) {
  tryCatch({
    base::saveRDS(dataframe, file = path, compress = "xz")
    message("Archivo RDS guardado correctamente: ", path)
    return(TRUE)  # Return TRUE if successful
  }, error = function(e) {
    stop("Error al guardar el archivo RDS: ", conditionMessage(e))
  })
}



#' Obtener configuración de GitHub
#'
#' Esta función devuelve la configuración del repositorio de GitHub, incluyendo el nombre del repositorio,
#' la rama a consultar y la ruta remota dentro del repositorio.
#'
#' @return Una lista con los elementos:
#'   \item{repo}{Nombre del repositorio en GitHub.}
#'   \item{branch}{Rama en la que se realizarán las consultas.}
#'   \item{remote_path}{Ruta dentro del repositorio donde están los archivos.}
#' @export
#'
#' @examples
#' config <- obtener_configuracion_github()
#' print(config$repo)        # "TRACE-LAC/labrep"
#' print(config$branch)      # "feat-github-api"
#' print(config$remote_path) # "inst/extdata/data-historica-periodoepi"
obtener_configuracion_github <- function() {
  repo <- "TRACE-LAC/labrep"  # Nombre del repositorio en GitHub
  branch <- "feat-github-api" # Rama a consultar
  remote_path <- "inst/extdata/data-historica-periodoepi" # Carpeta en GitHub
  
  return(list(repo = repo, branch = branch, remote_path = remote_path))
}


#' Autenticar en GitHub
#'
#' Esta función configura la autenticación en GitHub mediante un token personal de acceso (PAT).
#'
#' @param token Cadena de texto que contiene el token de autenticación de GitHub.
#'
#' @return Un valor lógico: `TRUE` si la autenticación se configuró correctamente, `FALSE` si el token está vacío.
#' @export
#'
#' @examples
#' autenticar_github("mi_token_secreto") # TRUE si el token es válido, FALSE si está vacío
autenticar_github <- function(token) {
  # Ensure the token is a non-empty string
  if (!is.character(token) || nzchar(token) == FALSE) {
    stop("Error: El token de GitHub está vacío o no es válido. Verifica tu configuración.")
  }
  
  # Set the GitHub token as an environment variable
  Sys.setenv(GITHUB_PAT = token)
  message("Autenticación en GitHub configurada correctamente.")
  
  return(TRUE)
}




#' Crear directorio local
#'
#' Esta función verifica si un directorio local existe y lo crea si no está presente.
#'
#' @param local_library_path Ruta del directorio que se desea verificar o crear.
#'
#' @return Un valor lógico: `TRUE` si el directorio fue creado, `FALSE` si ya existía.
#' @export
#'
#' @examples
#' crear_directorio("data/historica") # TRUE si se creó, FALSE si ya existía
crear_directorio <- function(local_library_path) {
  if (!dir.exists(local_library_path)) {
    dir.create(local_library_path, recursive = TRUE)
    message("Directorio local creado: ", local_library_path)
    return(TRUE)  # Return TRUE if the directory was created
  }
  return(FALSE)  # Return FALSE if the directory already existed
}




#' Obtener lista de archivos remotos en GitHub
#'
#' Esta función obtiene la lista de archivos en una carpeta específica dentro de un repositorio de GitHub.
#'
#' @param repo Nombre del repositorio en formato `"usuario/repositorio"`.
#' @param remote_path Ruta dentro del repositorio donde se encuentran los archivos.
#' @param branch Nombre de la rama en la que se realizará la consulta.
#' @param token Token de autenticación de GitHub para acceder a los archivos.
#'
#' @return Una lista con dos elementos:
#'   \item{nombres}{Vector con los nombres de los archivos remotos.}
#'   \item{urls}{Vector con las URLs de descarga de los archivos.}
#' @export
#'
#' @examples
#' obtener_archivos_remotos("TRACE-LAC/labrep", "inst/extdata/data-historica-periodoepi", "feat-github-api", "mi_token")
obtener_archivos_remotos <- function(repo, remote_path, branch, token) {
  # Attempt to fetch the list of files from the remote repository
  remote_files <- tryCatch({
    gh::gh("GET /repos/{repo}/contents/{remote_path}", 
           repo = repo, remote_path = remote_path, ref = branch, .token = token)
  }, error = function(e) {
    stop(sprintf("Error: No se encontró la carpeta '%s' en el repositorio '%s', rama '%s'.", 
                 remote_path, repo, branch))
  })
  
  # Ensure that the response is a list of files
  if (!is.list(remote_files) || length(remote_files) == 0) {
    message(sprintf("Advertencia: La carpeta '%s' en el repositorio '%s', rama '%s' está vacía o no existe.", 
                    remote_path, repo, branch))
    return(list(nombres = character(0), urls = character(0)))
  }
  
  # Filter only valid files (with a download URL)
  remote_files <- Filter(function(file) !is.null(file$download_url), remote_files)
  
  # Extract filenames and URLs
  remote_file_names <- vapply(remote_files, function(file) file$name, character(1))
  remote_file_urls <- vapply(remote_files, function(file) file$download_url, character(1))
  
  # Return structured output
  return(list(nombres = remote_file_names, urls = remote_file_urls))
}





#' Obtener lista de archivos locales
#'
#' Esta función devuelve la lista de archivos dentro de un directorio local específico.
#'
#' @param local_path Ruta del directorio local donde se buscarán los archivos.
#'
#' @return Un vector con los nombres de los archivos dentro del directorio especificado.
#' @export
#'
#' @examples
#' obtener_archivos_locales("data/historica") 
obtener_archivos_locales <- function(local_library_path) {
  # List all files in the directory (excluding subdirectories)
  local_files <- list.files(local_library_path, full.names = FALSE, recursive = FALSE)
  
  # Return the list of files
  return(local_files)
}





#' Comparar archivos locales y remotos
#'
#' Esta función compara la lista de archivos locales con la lista de archivos remotos en GitHub 
#' e identifica cuáles archivos están en un lugar pero no en el otro.
#'
#' @param local_files Vector con los nombres de los archivos en la carpeta local.
#' @param remote_file_names Vector con los nombres de los archivos en la carpeta remota de GitHub.
#'
#' @return Una lista con dos elementos:
#'   \item{faltan_en_remoto}{Archivos que están en local pero no en GitHub.}
#'   \item{faltan_en_local}{Archivos que están en GitHub pero no en local.}
#' @export
#'
#' @examples
#' archivos_locales <- c("archivo1.csv", "archivo2.csv")
#' archivos_remotos <- c("archivo2.csv", "archivo3.csv")
#' comparar_y_mostrar_diferencias(archivos_locales
comparar_y_mostrar_diferencias <- function(local_files, remote_file_names) {
  # Compare lists
  missing_in_remote <- setdiff(local_files, remote_file_names)  # Files in local but not on GitHub
  missing_in_local <- setdiff(remote_file_names, local_files)  # Files on GitHub but not locally
  
  # Display differences in a structured format
  if (length(missing_in_remote) > 0) {
    message("Archivos en LOCAL pero NO en GitHub:")
    message(paste(missing_in_remote, collapse = "\n"))
  } else {
    message("Todos los archivos locales están en GitHub.")
  }
  
  if (length(missing_in_local) > 0) {
    message("Archivos en GitHub pero NO en LOCAL:")
    message(paste(missing_in_local, collapse = "\n"))
  } else {
    message("Todos los archivos en GitHub están en local.")
  }
  
  # Return a list with differences
  return(list(faltan_en_remoto = missing_in_remote, faltan_en_local = missing_in_local))
}



#' Descargar archivos faltantes desde GitHub
#'
#' Esta función descarga archivos que están en GitHub pero no en la carpeta local.
#'
#' @param missing_in_local Vector con los nombres de los archivos que faltan en local.
#' @param archivos_remoto Lista con dos elementos: 
#'   \item{nombres}{Vector con los nombres de los archivos en GitHub.}
#'   \item{urls}{Vector con las URLs de descarga de los archivos en GitHub.}
#' @param local_library_path Ruta del directorio local donde se guardarán los archivos descargados.
#'
#' @return Un vector con los nombres de los archivos descargados.
#' @export
#'
#' @examples
#' archivos_faltantes <- c("archivo1.csv", "archivo2.csv")
#' archivos_remotos <- list(nombres = c("archivo1.csv", "archivo2.csv"), 
#'                          urls = c("https://github.com/user/repo/raw/main/archivo1.csv", 
#'                                   "https://github.com/user/repo/raw/main/archivo2.csv"))
#' descargar_archivos_faltantes(archivos_faltantes, archivos_remotos, "data/historica")
descargar_archivos_faltantes <- function(missing_in_local, archivos_remoto, local_library_path) {
  # If no files are missing, exit early
  if (length(missing_in_local) == 0) {
    message("Todos los archivos en GitHub están en local.")
    return(character(0))  # Return an empty character vector
  }
  
  message("Descargando archivos faltantes desde GitHub...")
  
  downloaded_files <- c()  # Store successfully downloaded files
  
  for (file in missing_in_local) {
    # Find the corresponding URL
    file_url <- archivos_remoto$urls[archivos_remoto$nombres == file]
    
    # Ensure the URL is valid (not NULL, not empty)
    if (length(file_url) > 0 && nzchar(file_url)) {
      dest_file <- file.path(local_library_path, file)
      
      tryCatch({
        utils::download.file(file_url, destfile = dest_file, mode = "wb")
        message(paste("Descargado:", file))
        downloaded_files <- c(downloaded_files, file)  # Add to successful list
      }, error = function(e) {
        message(paste("Error descargando:", file, "-", conditionMessage(e)))
      })
    } else {
      message(paste("URL no encontrada para el archivo:", file))
    }
  }
  
  message("Descarga completada.")
  
  # Return only successfully downloaded files
  return(downloaded_files)
}





#' Codificar un archivo RDS en Base64
#'
#' Esta función lee un archivo RDS en formato binario y lo codifica en Base64.
#'
#' @param local_rds_path Ruta del archivo RDS a codificar.
#'
#' @return Una cadena de texto con el contenido del archivo RDS codificado en Base64.
#' @export
#'
#' @examples
#' codificado <- codificar_rds_base64("datos.rds")
#' print(codificado) # Devuelve una cadena en Base64
codificar_rds_base64 <- function(local_rds_path) {
  # Leer el archivo RDS en formato binario
  rds_content <- readBin(local_rds_path, "raw", file.info(local_rds_path)$size)
  
  # Codificar el contenido en Base64 utilizando jsonlite::base64_enc
  encoded_content <- jsonlite::base64_enc(rds_content)
  
  # Retornar el contenido codificado
  return(encoded_content)
}




#' Obtener el SHA de un archivo en GitHub
#'
#' Esta función recupera el SHA de un archivo específico en un repositorio de GitHub.
#'
#' @param repo Nombre del repositorio en formato `"usuario/repositorio"`.
#' @param remote_file_path Ruta del archivo dentro del repositorio.
#' @param branch Nombre de la rama en la que se realizará la consulta.
#' @param token Token de autenticación de GitHub para acceder al repositorio.
#'
#' @return Una cadena de texto con el SHA del archivo si existe, o `NULL` si no se encuentra.
#' @export
#'
#' @examples
#' obtener_sha_archivo("TRACE-LAC/labrep", "inst/extdata/data-historica-periodoepi/archivo.rds", "feat-github-api", "mi_token")
obtener_sha_archivo <- function(repo, remote_file_path, branch, token) {
  # Obtener información del archivo en GitHub con manejo de errores
  file_info <- tryCatch(
    expr = gh::gh(paste0("GET /repos/", repo, "/contents/", remote_file_path, "?ref=", branch), .token = token),
    error = function(e) return(NULL)
  )
  
  # Extraer el SHA si el archivo ya existe, de lo contrario, retornar NULL
  if (!is.null(file_info)) {
    return(file_info$sha)
  }
  
  return(NULL)
}




#' Generar ruta remota del archivo en GitHub
#'
#' Esta función construye la ruta de un archivo dentro de la estructura del repositorio en GitHub.
#'
#' @param nombre_archivo Nombre del archivo que se almacenará en el repositorio.
#' @param libreria Nombre de la carpeta dentro de `inst/extdata` donde se almacenará el archivo. 
#'        Por defecto, `"data-historica-periodoepi"`.
#'
#' @return Una cadena de texto con la ruta del archivo dentro del repositorio de GitHub.
#' @export
#'
#' @examples
#' generar_ruta_remota("archivo.rds")
#' # Devuelve: "inst/extdata/data-historica-periodoepi/archivo.rds"
generar_ruta_remota <- function(nombre_archivo, libreria = "data-historica-periodoepi") {
  # Validar que el nombre del archivo no esté vacío
  if (missing(nombre_archivo) || nombre_archivo == "") {
    stop("El argumento 'nombre_archivo' no puede estar vacío.")
  }
  # Construir la ruta usando file.path() 
  remote_file_path <- file.path("inst", "extdata", libreria, nombre_archivo)
  # Retornar la ruta generada
  return(remote_file_path)
}




#' Subir o actualizar un archivo en GitHub
#'
#' Esta función sube un archivo codificado en Base64 a un repositorio de GitHub. 
#' Si el archivo ya existe, se actualiza utilizando su SHA.
#'
#' @param repo Nombre del repositorio en formato `"usuario/repositorio"`.
#' @param remote_file_path Ruta del archivo dentro del repositorio.
#' @param encoded_content Contenido del archivo codificado en Base64.
#' @param branch Nombre de la rama donde se subirá el archivo.
#' @param token Token de autenticación de GitHub para acceder al repositorio.
#' @param file_sha (Opcional) SHA del archivo si ya existe, necesario para actualizarlo.
#'
#' @return La respuesta de la API de GitHub si la subida es exitosa, o `NULL` en caso de error.
#' @export
#'
#' @examples
#' subir_o_actualizar_archivo("TRACE-LAC/labrep", "inst/extdata/data-historica-periodoepi/archivo.rds",
#'                            "contenido_base64", "feat-github-api", "mi_token", "sha_existente")
subir_o_actualizar_archivo <- function(repo, remote_file_path, encoded_content, branch, token, file_sha = NULL) {
  # Attempt to upload or update the file in GitHub
  respuesta <- tryCatch({
    gh::gh(
      "PUT /repos/{repo}/contents/{remote_file_path}",
      repo = repo,
      remote_file_path = remote_file_path,
      message = "Uploading RDS DataFrame",
      content = encoded_content,
      branch = branch,
      sha = file_sha,  # This is either NULL (new file) or a SHA (update file)
      .token = token
    )
  }, error = function(e) {
    message("Error al subir el archivo: ", conditionMessage(e))
    return(NULL)
  })
  
  return(respuesta)
}








