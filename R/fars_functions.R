#' fars_read - reading data file
#'
#' This is a function that read the dedicated file in CSV format and output
#' as a \code{\link{dplyr}} \code{\link{tbl_df}}.
#'
#' @param filename A character string that specific the file to read
#' 
#' @return This function returns the data file as a \code{\link{dplyr}} \code{\link{tbl_df}}.
#'
#' @note Error when file does not exist
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' myfile <- "accident_2014.csv.gz2"
#' \dontrun{dataset <- fars_read(myfile)}
#' \dontrun{dataset <- fars_read("accident_2013.csv.gz2")}
#'
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename - create a file name for the compressed file
#'
#' This function takes the year of the accident to generate a
#' filename for the compressed file with the year on it.
#'
#' @param year Year of the accident, need to be able to converted to an integer
#' 
#' @return This function returns the accident file name in compressed format (surffix .csv.bz2) as a string.
#'
#' @examples
#' my_accident <- make_filename(2014)
#' make_filename("2013")
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years - read files with all the years
#'
#' This function takes a list of years of the accidents 
#' and read all the coorisponding files.
#'
#' @param years List of years whcih the files will be read.
#' 
#' @return a list of \code{\link{dplyr}} \code{\link{tbl_df}} which shows
#'    the month there is an accident in that year.
#'
#' @note Error when file does not exist or cannot be read
#'
#' @importFrom dplyr mutate select
#' @importFrom tidyr %>%
#'
#' @examples
#' \dontrun{recent_accident <- fars_read_years("2015")}
#' \dontrun{other_accident <- fars_read_years(c(2013,2014))}
#'
#' @export

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

#' fars_summarize_years - read files with all the years
#'
#' This function takes a list of years of the accidents,
#' read all the coorisponding files and summarize how many
#' accients in each month in each year specified.
#'
#' @param years List of years whcih the files will be read.
#' 
#' @return a \code{\link{dplyr}} \code{\link{tbl_df}} which summarize
#'    number of accident in each the month in each year specified.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr %>% spread
#'
#' @examples
#' \dontrun{recent_accidents_report <- fars_summarize_years("2015")}
#' \dontrun{fars_summarize_years(c(2013,2014))}
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state - plot location of all accidents for a specific state in a specific year
#'
#' This function takes a a specific state number and a specific year,
#' read the file of that year and plot the location of all accident happened
#' in that state in that year on a map.
#'
#' @param state.num The specific number whcih represend the state where the accidents occur.
#' @param year The specific year when the accidents occur, need to be able to converted to an integer
#' 
#' @return a graph which shows the locations of all accidents happened in a specific state
#'    in a specific year. The locations are shown in dots on the map of the state.
#'
#' @note Error when the state number is invalid or there's no accidents in that state in that year
#'
#' @importFrom dplyr filter
#' @importFrom tidyr %>%
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(23,2013)}
#' \dontrun{fars_map_state("10","2014")}
#'
#' @export

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
