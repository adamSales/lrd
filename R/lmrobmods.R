##' Wrapper to `lmrob.S` pegging the first regressor's coefficient at 0.
##' To get this effect, first feed `lmrob.S.mod` to `lmrob` in "init" slot.  Ignore cov returned
##' as part of result; instead, change its `ctrl$method` value to 'SM', then call `robustbase:::.vcov.avar1` on it. 
##'
##' In the intended context, the first regressor is a categorical variable, and the main point of the
##' regression is to test whether its coefficient(s) is (are) 0. The remaining independent variables are likely
##' to be measurement variables, so easier for the S estimator to handle (I think).  It's a neat coincidence
##' that fixing first stage regression coefficients doesn't affect the sandwich standard error calculation
##' for the second stage.
##'
##' Special case in which nothing is done to change `lmrob.S` behavior: if the
##' the design matrix was so singular that `robustbase::lmrob`'s pre-initial lm.fit
##' pivoted out the first regressor, then this doesn't zero out any other regressors.
##' I don't know whether this can occur with multiple regressors. 
##' 
##' @title Initial S-estimates setting coefficient of the first regressor to 0
##' @param x 
##' @param ...  additional parameters for `lmrob.S` 
##' @return whatever `lmrob.S` returns
##' @author Ben B Hansen
lmrob.S.mod <- function (x, ...)
    {
        stopifnot(!is.null(x))
        zerocols <- attr(x, "assign") == 1L
        thecoefs <- numeric(ncol(x))
        x <- x[ , !zerocols, drop=FALSE]
        init <- robustbase::lmrob.S(x, ...)

        if ( any(zerocols) ) {
            thecoefs[!zerocols] <- init$coef
            init$coef <- thecoefs
        }
        return(init)
    }
