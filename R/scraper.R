#' @title Retrieve UGent author IDs
#'
#' @description Retrieves all the author IDs by parsing the biblio webpage.
#'
#' @return a character vector containing all UGent author IDs
#' @export
#'
#' @importFrom pbapply pblapply
#' @importFrom xml2 read_html
#' @importFrom rvest html_attr html_nodes
#'
#' @examples
#' ids <- retrieve.ids()
#' ids[1:10]
retrieve.ids <- function() {
  cat("Retrieving UGent IDs\n")
  links <- unlist(pbapply::pblapply(LETTERS, function(chr) {
    ugent <- xml2::read_html(paste0("https://biblio.ugent.be/person?browse=", chr))
    links <- rvest::html_attr(rvest::html_nodes(ugent, ".list li a"), "href")
    links
  }))
  links <- unique(links)
  gsub("^.*/([0-9]*)$", "\\1", links)
}


#' @title Retrieve meta data of UGent author IDs
#'
#' @description Retrieves all the meta data of given UGent author IDs by parsing the biblio webpage.
#' If run for all UGent IDs, this might take a very long time. In order not to lose progress,
#' it's advised to retrieve all this data in chunks of 100 IDs, and if something
#' goes wrong to try that chunk again.
#'
#' @param ids a character vector containing UGent author IDs
#'
#' @return a data frame containing information pertaining the given UGent authors
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_node html_nodes
#' @importFrom pbapply pblapply
#'
#' @examples
#' ids <- c("001997743793", "801001497901", "801001188309")
#' person.data <- retrieve.person.data(ids)
retrieve.person.data <- function(ids) {
  check.text <- function(x) if (length(x) > 0) x else NA

  cat("Retrieving person data of ", length(ids), " authors\n", sep="")
  dplyr::bind_rows(pbapply::pblapply(ids, function(id) {
    url <- paste0("https://biblio.ugent.be/person/", id)

    link <- xml2::read_html(url)
    name <- rvest::html_text(rvest::html_node(link, "h2"))
    department <- check.text(rvest::html_text(rvest::html_nodes(link, ".org")))
    street <- check.text(rvest::html_text(rvest::html_nodes(link, ".adr .street-address")))
    postalcode <- check.text(rvest::html_text(rvest::html_nodes(link, ".adr .postal-code")))
    locality <- check.text(rvest::html_text(rvest::html_nodes(link, ".adr .locality")))
    email <- check.text(rvest::html_text(rvest::html_nodes(link, ".email")))

    data.frame(id, name, department, street, postalcode, locality, email, stringsAsFactors = F)
  }))
}


#' @title Retrieve publications of UGent authors
#'
#' @description Retrieves the publications of given UGent author IDs by parsing the biblio webpage.
#' If run for all UGent IDs, this might take a very long time. In order not to lose progress,
#' it's advised to retrieve all this data in chunks of 100 IDs, and if something
#' goes wrong to try that chunk again.
#'
#' @param ids a character vector containing UGent author IDs
#'
#' @return a list containing a publications data frame and a list containing each publication's authors
#' @export
#'
#' @importFrom dplyr bind_rows group_by slice ungroup
#' @importFrom pbapply pblapply
#' @importFrom RCurl getURL
#'
#' @examples
#' ids <- c("001997743793", "801001497901", "801001188309")
#' person.data <- retrieve.publication.data(ids)
retrieve.publication.data <- function(ids) {
  cat("Retrieving publication data of ", length(ids), " authors \n", sep = "")
  publications <- dplyr::bind_rows(pbapply::pblapply(ids, function(id) {
    csv   <- RCurl::getURL(paste0("https://biblio.ugent.be/publication/export?q=", id, "&sort=year.desc&format=csv"))
    if (csv != "") {
      read.table(text = csv, sep = ",", header = T, fill = T, quote = "\"", stringsAsFactors = F, colClasses = "character")
    } else {
      NULL
    }
  }))

  publications <- dplyr::ungroup(dplyr::slice(dplyr::group_by_(publications, "id"), 1))

  cat("Parsing author lists\n")
  publication.authors <- pbapply::pblapply(seq_len(nrow(publications)), function(i) {
    p <- publications[i,]
    ugent <- dplyr::bind_rows(lapply(strsplit(p$ugent_author, " ; ")[[1]], function(x) {
      last.name <- gsub("^([^,\\(]*),? ?.*$", "\\1", x)
      first.name <- gsub("^[^,\\(]*,? ?([^\\(]*) \\(.*\\)$", "\\1", x)
      full.name <- gsub("^([^\\(]*) \\(.*\\)$", "\\1", x)
      full.name2 <- paste0(first.name, " ", last.name)
      id <- gsub("^[^,\\(]*,? ?[^\\(]* \\(([^@]*)@?.*\\)$", "\\1", x)
      code <- gsub("^[^,\\(]*,? ?[^\\(]* \\([^@]*@?(.*)\\)$", "\\1", x)
      data.frame(last.name, first.name, full.name, full.name2, id, code, stringsAsFactors = F)
    }))
    other <- strsplit(p$author, " ; ")[[1]]
    other <- other[!other %in% ugent$full.name]

    list(id = p$id, ugent.authors = ugent, other.authors = other)
  })

  names(publication.authors) <- publications$id

  list(publications = publications, publication.authors = publication.authors)
}

