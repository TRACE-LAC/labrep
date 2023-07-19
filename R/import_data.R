import_data_viral_circulation <- function(report_data = c(), header = FALSE) {
  
  viral_circulation_data <- data.frame()
  
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      temp_data <- switch(  
        file_extension,  
        "xlsx" = readxl::read_excel(data_path, col_names = F, skip = 3),  
        "csv" = utils::read.csv(data_path, header = header, skip = if (header) 0 else 3)
      )
      if (!header) {
        temp_data <- row_to_header(data = temp_data)
        viral_circulation_data <- rbind(viral_circulation_data, temp_data)
      }
      else {
        viral_circulation_data <- temp_data
      }
    }
  }
  return(viral_circulation_data)
}