#' Read data from a file
#'
#' This function reads a csv-formatted data from a given file.
#' If the given file does not exist, the execution is stopped and an error message is given.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A name of the file that contains the data to be read
#'
#' @return This function returns the data as a tibble object
#'
#' @examples
#' fars_read("datafile.csv")

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a filename
#'
#' This function makes a name for a file referring to accidents during a given year.
#' The year is given as a parameter to he function.
#'
#' @param year A certain year to which the filename refers
#'
#' @return This function returns a character vector of a form "accident_YEAR.csv.bz2"
#'
#' @examples
#' make_filename(2020)

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read data from files referring to certain years
#'
#' This function reads csv-formatted accident data from files that refer to given years
#' ie. they contain data from the given years, If one of the files can not be readed, the function
#' throws a warning message and returns NULL.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param years A vector of years
#'
#' @return This function returns the data as a tibble object containing columns MONTH and year
#'
#' @examples
#' fars_read_years(c(2019, 2020))

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize (count the fatal injuries) the data by year and month given specific years
#'
#' This function reads data from files specified by the given years and counts
#' the number of accidents with fatal injuries by year and month.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @param years A vector of years
#'
#' @return This function returns a tibble object which gives the summary by year and month
#'
#' @examples
#' fars_summarize_years(2019)
#' fars_summarize_years(c(2019, 2020))

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Illustrate data with a map
#'
#' This is a function that illustrates data with the given state.num and year by a map.
#' If data does not contain the given state.num, the execution of the function is stopped
#' and "invalid STATE number" message is given. If the given state.num exists in the data
#' but there is no observations, the function gives a message "no accidents
#' to plot".
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num An identifier for the state
#' @param year A year of the data which is going to be illustrated
#'
#' @return This function plots the data
#'
#' @examples
#' fars_map_state(1, 2020)

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
