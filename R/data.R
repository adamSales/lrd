#' Data from Lindo et al (2010) academic probation study
#'
#' Data set with demographics, academic achievement info on
#' 42 thousand students at an unnamed Canadian university with three campuses.
#' Data are most thouroughly documented in Section III of that paper (pp. 100-101).
#' (Actually only the column headers of the data set are bundled here
#' -- for the actual data, see URL below.)
#'
#' @format A data frame of  42187 rows and 90 variables, including:
#' \describe{
#'     \item{left_school} 1 if the student left school
#'     \item{dist_from_cut} The running variable, i.e. gpa - cutpoint
#'     \item{gpalscutoff} Whether students 1st year GPA fell to left of cutoff
#'     \item{hsgrade_pct} Percentile of High-School GPA for universally-taken courses
#'     \item{lhsgrade_pct} Logit of hsgrade_pct
#'     \item{totcredits_year1} Credits attempted in first year
#'     \item{male} 1 if student is male, 0 otherwise
#'     \item{loc_campus1} 1 if student attended campus 1 (of three)
#'     \item{loc_campus2} 1 if student attended campus 2 (of three)
#'     \item{bpl_north_america} 1 if student was born in North America
#'     \item{english} 1 if student's first language was English
#'     \item{age_at_entry} Age at entry to college
#'     \item{nextGPA} GPA in subsequent semester, either summer or fall
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
##' @author lrd author 2
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
