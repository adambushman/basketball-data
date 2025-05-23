library('rvest')


mocks <- data.frame(
  source = "Bleacher Report", 
  selector = ".MuiTypography-bp_small__headings__title__large", 
  page = c(
    "https://bleacherreport.com/articles/25193838-2025-nba-mock-draft-new-trade-ideas-cooper-flagg-projections-after-combine"
  )
)


pull_headings <- function(page, selector) {
  data <- 
    rvest::read_html(page) |>
    rvest::html_elements(selector) |>
    rvest::html_text()
}



get_name <- function(x) {
  start = stringr::str_locate(x, ":")[1] + 2
  cutoffs = stringr::str_locate_all(x, "\\(")[[1]]
  end = cutoffs[length(cutoffs)/2,1] - 2
  name = stringr::str_sub(x, start, end)
  unname(name)
  return(name)
}

cleaned_data <- data[3:(length(data)-1)]

fresh <- vapply(cleaned_data, get_name, character(1))
real <- unname(fresh)


data.frame(real)
