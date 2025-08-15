# custom function for concurrent writing in SQLite database
dbWriteTable_ <- function(db_connection, table_name_, data_frame_,
                          append_ = FALSE, overwrite_ = FALSE) {
  write_not_finished <- TRUE
  while (write_not_finished) {
    tryCatch(
      {
        dbWriteTable(db_connection, table_name_, data_frame_,
                     append = append_, overwrite = overwrite_,
                     row.names = FALSE
        )
        write_not_finished <- FALSE
      },
      error = function(err) {
        print(paste0("insertion issue for ", table_name_))
      }
    )
  }
  return(NULL)
}
