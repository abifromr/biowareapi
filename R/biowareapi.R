query_map_bioware <- readr::read_csv(system.file('extdata',
                                                'query_map_bioware.csv',
                                                package = 'biowareapi'))
usethis::use_data(query_map_bioware, overwrite = TRUE)

#' Query Map for bioware API
#'
#' The object documents the different types of requests and their query
#' paramaters that are allowed by the API.
"query_map_bioware"
