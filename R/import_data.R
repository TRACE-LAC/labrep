#' @title Importar los datos de la circulación viral
#' @export
import_data_viral_circulation <- function(report_data = NULL,
                                          header = FALSE,
                                          skip = 3,
                                          col_names = FALSE,
                                          sheet) {
  viral_circulation_data <- data.frame()
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      temp_data <- switch(
        file_extension,
        "xlsx" = readxl::read_excel(data_path,
                                    col_names = col_names,
                                    skip = skip,
                                    sheet = sheet),
        "csv" = utils::read.csv(data_path, header = header,
                                skip = if (header) 0 else 3)
      )
      if (!header) {
        temp_data <- row_to_header(data = temp_data)
        viral_circulation_data <- rbind(viral_circulation_data, temp_data)
      } else {
        viral_circulation_data <- temp_data
      }
    }
  }
  return(viral_circulation_data)
}

#' @title Extraer todas las tablas de una hoja de Excel
#'
#' @description
#' Detecta y extrae múltiples tablas dentro de una hoja de Excel, identificando 
#' separaciones mediante filas y columnas en blanco.
#'
#' @param file_name Ruta del archivo de Excel.
#' @param sheet_name Nombre de la hoja de donde extraer las tablas.
#'
#' @return Lista de dataframes, donde cada elemento representa una tabla identificada dentro de la hoja.
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

#' @title Obtener una tabla específica de una lista de tablas
#'
#' @description
#' Extrae una tabla de una lista de tablas generada a partir de `get_all_tables`, 
#' seleccionándola por su índice en la lista.
#'
#' @param list_of_tables Lista de tablas, donde cada elemento es un dataframe.
#' @param indicator Índice numérico de la tabla que se desea extraer (debe estar dentro del rango de la lista).
#'
#' @return Un dataframe correspondiente a la tabla seleccionada.
#' @export
get_selected_table <- function(list_of_tables, indicator) {
  # Verificar que 'tables' es una lista
  if (!is.list(list_of_tables)) {
    stop("El argumento 'tables' debe ser una lista de tablas.")
  }
  
  # Verificar que el INDICADOR es válido
  if (indicator < 1 || indicator > length(list_of_tables)) {
    stop("El INDICADOR está fuera del rango de las tablas disponibles.")
  }
  
  # Extraer la tabla especificada
  selected_table <- list_of_tables[[indicator]]
  
  # Asegurarse de que la tabla es un data.frame o convertirla en uno si es necesario
  if (!is.data.frame(selected_table)) {
    selected_table <- as.data.frame(selected_table)
  }
  
  # Devolver la tabla seleccionada como data.frame
  return(selected_table)
}
