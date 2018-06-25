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
#' @importFrom purrr %>%
#'
#' @examples
#' ids <- retrieve.ids()
#' ids[1:10]
retrieve.ids <- function() {
  cat("Retrieving UGent IDs\n")
  links <- unlist(pbapply::pblapply(LETTERS, function(chr) {
    paste0("https://biblio.ugent.be/person?browse=", chr) %>%
      xml2::read_html() %>%
      rvest::html_nodes(".list li a") %>%
      rvest::html_attr("href") %>%
      keep(grepl("biblio", .))
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
#' @param mc_cores The number of cores to use, or a qsub qsub_config
#'
#' @return a data frame containing information pertaining the given UGent authors
#' @export
#'
#' @importFrom qsub is_qsub_config qsub_lapply
#' @importFrom dplyr bind_rows data_frame
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_node html_nodes
#' @importFrom purrr map_df
#'
#' @examples
#' ids <- c("001997743793", "801001497901", "801001188309")
#' person.data <- retrieve.person.data(ids)
retrieve.person.data <- function(ids, mc_cores = 1) {
  cat("Retrieving person data of ", length(ids), " authors\n", sep="")

  FUN <- function(id) {
    check.text <- function(x) if (length(x) > 0) x else NA

    data <- paste0("https://biblio.ugent.be/person/", id) %>%
      xml2::read_html()

    data_frame(
      id,
      name = data %>% html_node("h2") %>% html_text() %>% check.text(),
      department = data %>% html_nodes(".org") %>% html_text() %>% check.text(),
      street = data %>% html_nodes(".adr .street-address") %>% html_text() %>% check.text(),
      postalcode = data %>% html_nodes(".adr .postal-code") %>% html_text() %>% check.text(),
      locality = data %>% html_nodes(".adr .locality") %>% html_text() %>% check.text(),
      email = data %>% html_nodes(".email") %>% html_text() %>% check.text()
    )
  }

  if (qsub::is_qsub_config(mc_cores)) {
    bind_rows(qsub::qsub_lapply(
      X = ids,
      FUN = FUN,
      qsub_config = mc_cores,
      qsub_environment = c("ids", "FUN"),
      qsub_packages = c("tidyverse", "xml2", "rvest")
    ))
  } else {
    if (!is.numeric(mc_cores)) mc_cores <- 1
    purrr::map_df(ids, FUN, cl = mc_cores)
  }
}

#' @title Retrieve publications of UGent authors
#'
#' @description Retrieves the publications of given UGent author IDs by parsing the biblio webpage.
#' If run for all UGent IDs, this might take a very long time. In order not to lose progress,
#' it's advised to retrieve all this data in chunks of 100 IDs, and if something
#' goes wrong to try that chunk again.
#'
#' @param ids a character vector containing UGent author IDs
#' @param mc_cores The number of cores to use, or a qsub qsub_config
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
retrieve.publication.data <- function(ids, mc_cores = 1) {
  cat("Retrieving publication data of ", length(ids), " authors \n", sep = "")
  pub_fun <- function(id) {
    csv   <- RCurl::getURL(paste0("https://biblio.ugent.be/publication/export?q=", id, "&sort=year.desc&format=csv"))
    if (csv != "") {
      read.table(text = csv, sep = ",", header = T, fill = T, quote = "\"", stringsAsFactors = F, colClasses = "character")
    } else {
      NULL
    }
  }

  publications <-
    if (qsub::is_qsub_config(mc_cores)) {
      bind_rows(qsub::qsub_lapply(
        X = ids,
        FUN = pub_fun,
        qsub_config = mc_cores,
        qsub_environment = c("ids", "pub_fun"),
        qsub_packages = c("tidyverse", "xml2", "rvest")
      ))
    } else {
      if (!is.numeric(mc_cores)) mc_cores <- 1
      purrr::map_df(ids, pub_fun, cl = mc_cores)
    }

  publications <-
    publications %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

  cat("Parsing author lists\n")
  X <- seq_len(nrow(publications))
  pub_aut_fun <- function(i) {
    p <- publications[i,]
    ugent <- bind_rows(lapply(strsplit(p$ugent_author, " ; ")[[1]], function(x) {
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
  }

  publication.authors <-
    if (qsub::is_qsub_config(mc_cores)) {
      qsub::qsub_lapply(
        X = X,
        FUN = pub_aut_fun,
        qsub_config = mc_cores,
        qsub_environment = c("ids", "pub_aut_fun"),
        qsub_packages = c("tidyverse", "xml2", "rvest")
      )
    } else {
      if (!is.numeric(mc_cores)) mc_cores <- 1
      purrr::map(X, pub_aut_fun, cl = mc_cores)
    }

  names(publication.authors) <- publications$id

  list(publications = publications, publication.authors = publication.authors)
}

