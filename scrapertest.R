devtools::install_github("rcannood/scrapebiblio")

library(scrapebiblio)
ids <- retrieve.ids()
first.ten <- ids[1:10]
person.data <- retrieve.person.data(first.ten)
publication.data <- retrieve.publication.data(first.ten)

person.data
publication.data$publications
publication.data$publication.authors[[1]]
