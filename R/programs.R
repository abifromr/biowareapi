#' Find cohorts of locally conserved residues
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @seealso slimfinder
#' @seealso slimsearch
#' @seealso slimpred
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
slimprints <- function(uniprotid = NULL) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'slimprints',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/slimprints',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Proteome side motif search
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#' @param motif A \code{regex} for a motif to search.
#' @param taxonid A \code{numeric}. Default is 9606 NCBI species ID.
#' @param orthdb A \code{character} only 'Orthdb'
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @seealso slimprints
#' @seealso slimfinder
#' @seealso slimpred
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
slimsearch <- function(uniprotid = NULL, motif, taxonid = 9606,
                       orthdb = 'Orthdb') {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'slimsearch',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/slimsearch',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Short Linear Motif Finder
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#' @param dismask A \code{logical} default (TRUE) of disorder masking.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @seealso slimprints
#' @seealso slimsearch
#' @seealso slimpred
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
slimfinder <- function(uniprotid = NULL, dismask = TRUE) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'slimfinder',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/slimfinder',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Identify potential SLiM-like region in a protein sequence
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @seealso slimprints
#' @seealso slimfinder
#' @seealso slimsearch
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
slimpred <- function(uniprotid = NULL) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'slimpred',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/slimpred',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Simple motif comparison
#'
#' @param motifs A \code{regex} representing motifs.
#' @param cmotifs A \code{regex} representing motifs to compare to
#' \code{motifs}.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
comparimotif <- function(motifs = NULL, cmotifs = NULL) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'comparimotif',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/comparimotif',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Identify bioactive peptides in protein sequences
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
peploc <- function(uniprotid = NULL) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'peploc',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/peploc',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}

#' Generate alignment for orthologous proteins
#'
#' @param uniprotid A \code{character} vector of one or more UniProt ID.
#' @param taxonid A \code{numeric}. Default is 9606 NCBI species ID.
#'
#' @return A \code{tibble}.
#'
#' @source http://bioware.ucd.ie/~compass/biowareweb/
#'
#' @importFrom apihelpers make_query send_request format_content
#' @importFrom httr modify_url
#'
#' @export
gopher <- function(uniprotid = NULL, taxonid = 9606) {
  # collect arguments in a list
  param <- as.list(environment())

  # remove missing arguments
  ind <- unlist(lapply(param, is.name))
  param <- param[!ind]

  # construct query
  query <- make_query(
    request = 'gopher',
    parameters = param,
    query_map = query_map_bioware
  )

  # make url
  url <- modify_url('',
                    scheme = 'http',
                    hostname = 'bioware.ucd.ie',
                    path = 'rest/gopher',
                    query = query)

  # get response
  resp <- send_request(url)

  # format contents
  res <- format_content(resp)

  # return results
  return(res)
}
