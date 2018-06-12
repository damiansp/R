#' Read a file into R as a dataframe tibble object
#'
#' Load data from a specified file path into R as a dataframe tibble onject. 
#' Will throw an error if the file path does not exist, or if the file type is 
#' not compatible with read_csv()
#'
#' @parameter filename A character string indicating the path to the (.csv) file 
#'   to be loaded
#'
#' @return The loaded data as a dataframe tibble object
#'
#' @examples
#' fars_read('file_in_working_directory.csv')
#' fars_read('file/at/absolute/path.csv')
#' fars_read('../file/at/relative/path.csv')
fars_read <- function(filename) {
  if(!file.exists(filename)) {
    stop("file '", filename, "' does not exist")	
  }
  data <- suppressMessages({
    readr::read_csv(filename, progress=FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate a file name
#'
#' Generates a file name string of the format "accident_YYYY.csv.bz2" where YYYY 
#'   is a year
#'
#' @parameter year Numeric or character string indicating the year of the data
#'
#' @return A character string indicating the file name
#'
#' @examples
#' make_filename(2013)
#' make_filename('2014')
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data/accident_%d.csv.bz2", year) 
}


#' Read and format data for all years supplied
#'
#' Reads and formats all files "accident_YYYY.csv.bz2" for years YYYY
#' Will generate a warning if the file does not exist
#'
#' @parameter years Numeric or Character vector, with each element 
#'   indicating a year of data to be loaded and formatted
#'
#' @return a list of dataframe tibble objects with the month and year of the 
#'   data for each year passed in <years>
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2015))
#' fars_read_years(2013:2015)
fars_read_years <- function(years) {
  lapply(
    years, 
    function(year) {
      file <- make_filename(year)
      tryCatch({
        dat <- fars_read(file)
        dplyr::mutate(dat, year=year) %>% 
          dplyr::select(MONTH, year)
      }, 
      error = function(e) {
        warning("invalid year: ", year)
        return (NULL)
      })
    })
}


#' Read and format data into a single table for all years supplied
#'
#' Reads all files "accident_YYYY.csv.bz2" for years YYYY. Formats data into a 
#'   single table with summary values for each month and year
#'
#' @parameter years Numeric or Character vector, with each element 
#'   indicating a year of data to be loaded and formatted
#'
#' @return a datafram tibble object with months as rows and years as columns
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013, 2015))
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n=n()) %>%
    tidyr::spread(year, n)
}


#' Generates a map of accident locations for a given state and year
#'
#' Will throw an error if the state value is out of range, or a warning if the 
#'   year is out of range.  Will provide a message if there are no accidents to 
#'   plot for the given year and state.
#'
#' @parameter state.num Numeric (integer) between 1-51 indicating the 50 states 
#'   and Washington, DC.
#' @parameter year Numeric (integer) indicating one of the years in the data set
#'
#' @return NULL (generates a graphic in the current plotting device)
#' 
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(44, 2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  if(!(state.num %in% unique(data$STATE))) {
    stop("invalid STATE number: ", state.num)
  }
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return (invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim=range(LATITUDE, na.rm=TRUE),
    xlim = range(LONGITUD, na.rm=TRUE))
    graphics::points(LONGITUD, LATITUDE, pch=46)
  })
}
