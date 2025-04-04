#' @title Importar los datos de la circulación viral
#' @export
import_data_viral_circulation <- function(report_data = NULL,
                                          header = FALSE,
                                          skip = NULL,
                                          col_names = FALSE,
                                          sheet = NULL,
                                          dataset_name = NULL) {
  viral_circulation_data <- data.frame()
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  filmarray_sheets <-
    config::get(file = config_path, "filmarray_data")$file$sheets
  fci_datasets <- list()
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      if (file_extension == "xlsx") {
        if (is.null(sheet)) {
          i <- 1
          sheets <- readxl::excel_sheets(data_path)
          for (sheet in sheets) {
            temp_data <-
              readxl::read_excel(data_path,
                                 skip = filmarray_sheets$skip[i],
                                 sheet = sheet,
                                 col_types = "text")
            if (stringr::str_detect(sheet, filmarray_sheets$names[1])) {
              fci_datasets$filmarray <- temp_data
            }
            if (stringr::str_detect(sheet, filmarray_sheets$names[2])) {
              fci_datasets$panel <- temp_data
            }
            i <- i + 1
          }
        } else if (!is.null(skip)) {
          temp_data <-
            readxl::read_excel(data_path, skip = skip, sheet = sheet,
                               col_names = col_names)
        }
      }
      if (file_extension == "csv") {
        utils::read.csv(data_path, header = header,
                        skip = if (header) 0 else 3)
      }
    }
  }
  
  if (!is.null(dataset_name) && dataset_name == "fci") {
    return(fci_datasets)
  }
  
  if (!header) {
    temp_data <- row_to_header(data = temp_data)
    viral_circulation_data <- rbind(viral_circulation_data, temp_data)
  } else {
    viral_circulation_data <- temp_data
  }
  
  return(viral_circulation_data)
}

#' @title Obtener todas las tables de las bases historicas
#' @export
get_all_tables <- function(file_name, sheet_name) {
  # Leer los datos de la hoja especificada en el archivo
  data <-  readxl::read_excel(file_name, sheet = sheet_name)
  
  # Detectar filas y columnas en blanco
  blank_rows <- which(apply(data, 1, function(x) all(is.na(x))))
  blank_cols <- which(apply(data, 2, function(x) all(is.na(x))))
  
  # Agregar los límites de los datos como marcadores de fila/columna en blanco para asegurar que se procesen todas las secciones
  blank_rows <- c(0, blank_rows, nrow(data) + 1)
  blank_cols <- c(0, blank_cols, ncol(data) + 1)
  
  # Inicializar una lista para almacenar las tablas
  tables <- list()
  
  # Bucle sobre las secciones verticales definidas por las filas en blanco
  for (i in seq_along(blank_rows)[-length(blank_rows)]) {
    row_start <- blank_rows[i] + 1
    row_end <- blank_rows[i + 1] - 1
    if (row_start <= row_end) {
      # Extraer segmento vertical
      sub_df <- data[row_start:row_end, ]
      
      # Bucle sobre las secciones horizontales dentro de cada segmento vertical
      for (j in seq_along(blank_cols)[-length(blank_cols)]) {
        col_start <- blank_cols[j] + 1
        col_end <- blank_cols[j + 1] - 1
        if (col_start <= col_end) {
          # Extraer cada segmento de tabla
          table_part <- sub_df[, col_start:col_end] %>%
            filter(if_any(everything(), ~ !is.na(.))) %>%  # Remover filas en blanco
            select(where(~ any(!is.na(.))))  # Remover columnas en blanco
          
          # Agregar a la lista si no está vacío
          if (nrow(table_part) > 0 && ncol(table_part) > 0) {
            tables <- append(tables, list(table_part))
          }
        }
      }
    }
  }
  
  return(tables)
}

#' @title Extraer una tabla específica de la lista y devolverla como data.frame
#' @export
get_selected_table <- function(tables, INDICADOR) {
  # Verificar que 'tables' es una lista
  if (!is.list(tables)) {
    stop("El argumento 'tables' debe ser una lista de tablas.")
  }
  
  # Verificar que el INDICADOR es válido
  if (INDICADOR < 1 || INDICADOR > length(tables)) {
    stop("El INDICADOR está fuera del rango de las tablas disponibles.")
  }
  
  # Extraer la tabla especificada
  selected_table <- tables[[INDICADOR]]
  
  # Asegurarse de que la tabla es un data.frame o convertirla en uno si es necesario
  if (!is.data.frame(selected_table)) {
    selected_table <- as.data.frame(selected_table)
  }
  
  # Devolver la tabla seleccionada como data.frame
  return(selected_table)
}
