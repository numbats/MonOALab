# Find CRAN packages with author containing <name>
find_cran_packages <- function(name) {
  pkgsearch::ps(name, size = 100) %>%
    filter(purrr::map_lgl(
      package_data, ~ grepl(name, .x$Author, fixed = TRUE)
    )) %>%
    select(package) %>%
    pull(package)
}

# Monthly download counts from packages in <x>
cran_downloads <- function(x) {
  # Compute monthly download counts
  down <- cranlogs::cran_downloads(x, from = "2000-01-01") %>%
    as_tibble() %>%
    mutate(month = tsibble::yearmonth(date)) %>%
    group_by(month) %>%
    summarise(count = sum(count), package = x)
  # Strip out initial zeros
  first_nonzero <- down %>%
    filter(count > 0) %>%
    head(1)
  if (NROW(first_nonzero) > 0) {
    filter(down, month >= first_nonzero$month)
  } else {
    first_nonzero
  }
}

# Clean up author list from CRAN meta data
clean_authors <- function(x) {
  # Fix weird characters
  x <- gsub("<U+000a>", " ", x)
  # Add J to my name
  x <- gsub("Rob Hyndman", "Rob J Hyndman", x)
  # Fix Souhaib's name
  x <- gsub("Ben Taieb", "{Ben~Taieb}", x)
  # Replace R Core Team with {R Core Team}
  x <- gsub("R Core Team", "{R Core Team}", x)
  # Replace AEC
  x <- gsub("Commonwealth of Australia AEC", "{Commonwealth of Australia AEC}", x)
  # Replace ABS
  x <- gsub("Australian Bureau of Statistics ABS", "{Australian Bureau of Statistics ABS}", x)
  # Remove comments in author fields
  x <- gsub("\\([a-zA-Z0-9\\-\\s,&\\(\\)<>:/\\.']*\\)", " ", x, perl = TRUE)
  # Remove email addresses
  x <- gsub("<[a-zA-Z@.]*>", "", x, perl = TRUE)
  # Remove contribution classification
  x <- gsub("\\[[a-zA-Z, ]*\\]", "", x, perl = TRUE)
  # Remove github handles
  x <- gsub("\\([@a-zA-Z0-9\\-]*\\)", "", x, perl = TRUE)
  # Replace line breaks with "and"
  x <- gsub("\\n", " and ", x)
  # Replace commas with "and"
  x <- gsub(",", " and ", x)
  # Trim spaces
  x <- trimws(x)
  x <- gsub("  +", " ", x, perl = TRUE)
  # Remove duplicate ands
  x <- gsub("and and and ", "and ", x, perl = TRUE)
  x <- gsub("and and ", "and ", x, perl = TRUE)
  return(x)
}

clean_description <- function(x) {
  # Clean up as for authors
  # Add J to my name
  x <- gsub("Rob Hyndman", "Rob J Hyndman", x)
  # Remove line breaks
  x <- gsub("\\n", " ", x)
  # Trim spaces
  x <- trimws(x)
  # Find arXiv links
  x <- gsub("arXiv:", "https://arxiv.org/abs/", x, perl = TRUE)
  # Find doi links
  x <- gsub("doi:", "https://doi.org/", x, perl = TRUE)
  x <- gsub("DOI:", "https://doi.org/", x, perl = TRUE)
  # Fix weird characters
  x <- gsub("<U+000a>", " ", x, fixed=TRUE)
  return(x)
}

