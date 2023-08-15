# Code to select repositories of interest from re3data. Code developed by
# Justine Vandendorpe.

# 1. Set up ----

# 1.1. Load packages ====
library(httr)
library(purrr)
library(dplyr)
library(XML)

# 1.2. Disable the feature of turning character strings into factor variables ====
options(stringsAsFactors = FALSE)

# 2. Access re3data API ----
url <- 'https://www.re3data.org/api/v1/repositories'
repositories_md <- GET(url = url)
repositories_md_content <- rawToChar(repositories_md$content)

# 3. Parse XML to list ----
repositories_md_xml <- xmlParse(repositories_md_content)
repositories_md_list <- xmlToList(repositories_md_xml)

# 4. Repeat steps 2 and 3 for each repository ----
repositories_md_lists <- list()
for(i in 1:length(repositories_md_list)) {
  repo_href <- repositories_md_list[i]$repository$link[1]
  repo_url <- paste('https://www.re3data.org', repo_href, sep = '')
  repo_md <- GET(url = repo_url)
  repo_md_content <- rawToChar(repo_md$content)
  repo_xml <- xmlParse(repo_md_content)
  repo_list <- xmlToList(repo_xml)
  repositories_md_lists <- append(repositories_md_lists, list(repo_list))
}

# 5. Select repositories that match criteria ----
# Subject = life sciences
life_science_repo <- Filter(function(x) isTRUE(x$repository$subject$text ==
                                                 '2 Life Sciences'),
                            repositories_md_lists)
# Data access = open
open_access_repo <- Filter(function(x) isTRUE(x$repository$dataAccess == 'open'),
                           life_science_repo)
# Repository type = disciplinary
disciplinary_repo <- Filter(function(x) isTRUE(x$repository$type == 'disciplinary'),
                            open_access_repo)
# Data upload = restricted or open
registration_and_open_repo <- Filter(function(x) isTRUE(x$repository$dataUpload$dataUploadRestriction == 'registration'
                                                        || x$repository$dataUpload$dataUploadType == 'open'),
                                     disciplinary_repo)

# 6. Return a dataframe with variables of interest ----

# Repository name, URL, enhanced publication and PID system
iterations = length(registration_and_open_repo)
variables = 4
output <- matrix(ncol = variables, nrow = iterations)
for (i in 1:iterations) {
  output[i, ] <- c(registration_and_open_repo[[i]]$repository$repositoryName$text,
                   ifelse(is.null(registration_and_open_repo[[i]]$repository$repositoryURL),
                          "none",
                          registration_and_open_repo[[i]]$repository$repositoryURL),
                   registration_and_open_repo[[i]]$repository$enhancedPublication,
                   ifelse(is.null(registration_and_open_repo[[i]]$repository$pidSystem),
                          "none",
                          registration_and_open_repo[[i]]$repository$pidSystem))

}

#TODO: write a function for the next two steps
# Subjects
subject_lists <- list()
for (i in 1:length(registration_and_open_repo)) {
  subject_lists <- append(subject_lists,
                          list(c(modify_depth(registration_and_open_repo[[i]]$repository[names(registration_and_open_repo[[i]]$repository)
                                                                                         == 'subject'],
                                              1, 'text'))))
}
subject_lists <- lapply(subject_lists, function(x)
  x[grepl('^[2]\\d{1,2}\\s', x)]
  )
dim(subject_lists) <- c(length(subject_lists), 1)
output <- cbind(output, subject_lists)

# Countries
country_lists <- list()
for (i in 1:length(registration_and_open_repo)) {
  country_lists <- append(country_lists,
                          list(c(modify_depth(registration_and_open_repo[[i]]$repository[names(registration_and_open_repo[[i]]$repository) == 'institution'], 1, 'institutionCountry'))))
}
country_lists <- lapply(country_lists, function(x)
  unique(x, x$member)
  )
dim(country_lists) <- c(length(country_lists), 1)
output <- cbind(output, country_lists)

# Convert matrix as dataframe
selected_repo_df <- as.data.frame(output)

# Change column names
colnames(selected_repo_df) <- c('Name', 'URL', 'Enhanced Publication',
                                'PIDs used/provided', 'Subjects', 'Country')

# Change column order
selected_repo_df <- selected_repo_df[ , c('Name', 'URL', 'Subjects', 'Country',
                                          'Enhanced Publication',
                                          'PIDs used/provided')]

# 7. Check whether repository URLs are valid ----
selected_repo_df$url_status_code <- NA
for (i in 1:nrow(selected_repo_df)) {
  tryCatch({
    url <- as.character(selected_repo_df$URL[[i]])
    tmp <- GET(url)
    selected_repo_df$url_status_code[i] <- tmp$status_code
  }, error = function(e) {cat('ERROR:', conditionMessage(e), '\n')})
}
# selected_repo_df$url_status_code[is.na(selected_repo_df$url_status_code)] <-
#   'ERROR'

# # Select only repositories whose status code is 200 or other supposedly working
# # status code
# selected_repo_df <- selected_repo_df[selected_repo_df$url_status_code
#                                      %in% c(200, 400, 401, 403, 404, 502, 503),
#                                      ]

# For repositories whose url is supposedly not working, indicate in the column
# URL: "Currently unavailable"
selected_repo_df <- within(selected_repo_df, 
                           URL[url_status_code %in% c(410, 405, 500, NA)] 
                           <- 'Currently unavailable')

# Exclude column 'url_status_code'
selected_repo_df$url_status_code <- NULL

# 8. Save output as csv ----

selected_repo_df %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = ', ')) %>%
  arrange(Name) %>%
  write.csv(paste0('output/selected_repo_', Sys.Date(), '.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
