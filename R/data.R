#' Data from Lindo et al (2010) academic probation study
#'
#' Data set with demographics, academic achievement info on
#' 42 thousand students at 3 unnamed Canadian universities.
#' (Actually only the column headers of the data set are bundled here
#' -- for the actual data, see URL below.)
#'
#' @format A data frame of  42187 rows and 90 variables, including:
#' \describe{
#'     \item{left_school} 1 if the student left school
#'     \item{dist_from_cut} The running variable, i.e. gpa - cutpoint
#'     \item{gpalscutoff} Whether students 1st year GPA fell to left of cutoff
#'     \item{lhsgrade_pct}
#'     \item{totcredits_year1}
#'     \item{male}
#'     \item{loc_campus1}
#'     \item{loc_campus2}
#'     \item{bpl_north_america}
#'     \item{english}
#'     \item{age_at_entry}
#'     \item{nextGPA} 
#' }
#' @source \url{https://www.aeaweb.org/aej/app/data/2008-0202_data.zip}
"dat_excerpt" 

##' Retrieve Lindo et al replication materials, including data
##'
##' Downloads the data from the AEJ website.
##' 
##' The \code{whereto} parameter should be a character string not ending
##' in \sQuote{\code{/}} (the path separator).
##' 
##' @title Fetch Lindo et al data 
##' @param whereto path specification for directory to data file to
##' @return path of downloaded dta file
##' @author Ben B Hansen
##' @export
fetchLSOdata <- function(whereto="extdata")
{
    stopifnot(is.character(whereto), length(whereto)==1,
              substr(whereto, nchar(whereto), nchar(whereto))!="/")
    temp <- tempfile()
    download.file('https://www.aeaweb.org/aej/app/data/2008-0202_data.zip',temp)
    unzip(temp,'AEJApp2008-0202_data/data_for_analysis.dta',
          junkpaths=TRUE,exdir=extdata_dir)
    paste0(whereto, "/data_for_analysis.dta")
    }
