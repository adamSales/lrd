##' Differences of dependent variables and model predictions on a new data set
##'
##' the prediction residuals themselves will be variables that balance tests are applied to.
##' gradients of the prediction residuals will be used to build rows of the A21 matrix
##' in a double-decker sandwich.  The same double decker has another role for cross products of the first stage estimating
##' function and the second stage estimating function. Data points that contribute only to one or the other of these
##' make not contribution to this cross product, and can be ignored; thus the role for the partial mapping of
##' rows of one data frame into the other. 
##'
##' @title Prediction residuals
##' @param model Fitted model 
##' @param newdata New data set
##' @return list of: residpreds, a vector of prediction residuals;
##' gradmat, a matrix of gradients of predictions w/r/t model coeffs
##' @author Ben B Hansen
predresid <- function(model, newdata)
    {
        stopifnot(inherits(model, "lm") || inherits(model, 'lmrob') || inherits(model, "glm") )
        if (inherits(model, "glm")) {
            stopifnot(all(c('linkinv', 'mu.eta') %in% names(family(model))))
            linkinv <- family(model)$linkinv
            dmu.deta <- family(model)$mu.eta
        } else {
            ## Here we're assuming any model that's not a glm has to be linear
            linkinv <- function(x) x
            dmu.deta <- function(x) 1
        }
        ## model has to be an lm or glm, for now
        stopifnot(!missing(newdata), !is.null(newdata)) # for now
        tt <- terms(model)
        ## crib a little from `predict.lm()`:
        m <- model.frame(tt, newdata, na.action=na.fail, xlev=model$xlevels)
             if (!is.null(cl <- attr(tt, "dataClasses"))) 
                 .checkMFClasses(cl, m)
        X <- model.matrix(tt, m, contrasts.arg = model$contrasts)
        offset <- rep(0, nrow(X))
        if (!is.null(off.num <- attr(tt, "offset"))) 
            for (i in off.num) offset <- offset + eval(attr(tt, 
                "variables")[[i + 1]], newdata)
        if (!is.null(model$call$offset)) 
            offset <- offset + eval(model$call$offset, newdata)
        ## handle rank-deficient case by a variation on what's in `predict.lm`

        p <- model$rank
        beta <- coef(model)
        alias <- is.na(beta)
        if (p < ncol(X))
            {
                warning("rank-deficient fit, dropping some predictors")
                p1 <- seq_len(p)
                piv <- if (p) qr.lm(model)$pivot[p1] # will with work on lmrob's? On glms?
                linpred <- drop(X[, piv, drop=FALSE] %*% beta[piv])
                X <- X[, !alias, drop=FALSE]
            } else {
                X <- X[, !alias, drop=FALSE]
                linpred <- drop(X %*% beta[!alias])
            }
        linpred <- linpred + offset
        mupred <- if (inherits(model, "glm")) family(model)$linkinv(linpred) else linpred

        ## model's dependent variable has to appear in newdata
        y <- model.response(m)
        
        residpred <- y - mupred


        gradmat <- X * dmu.deta(linpred)
        list(residpreds=residpred, gradmat=gradmat)
    }

##' When predictions or residuals from the fit of a stage one model contribute as
##' independent variables to the fit of a stage 2 model, the corresponding sandwich calls for
##' cross products of the two sets of estimating functions.  If the set of observations model 1
##' was fit to doesn't overlap with those used in fitting of model 2, then this cross-product is 0,
##' but if the two fit sets overlap then the cross-product is taken over the intersection of observations
##' used for the first and the second fits. This function extracts estimating functions for the stage 1 fit,
##' gets rid of the entries corresponding to observations not used at stage 2 and rearranges the remaining
##' entries to match the order of stage 2 observations.  If stage 2 involves observations that didn't
##' figure in stage 1, then corresponding rows in the matrix produced by this function are set to 0. 
##'
##' .. content for \details{} ..
##' @title First-stage estimating functions, aligned to second-stage estimating functions
##' @param model 
##' @param id.newdata common case IDs, aligned with data contributing to second stage fit
##' @param id.model common case IDs, aligned with data from first stage fit
##' @return Matrix of estimating function entries from a 1st stage fit, but with rows ordered and padded
##' to match data used for 2nd stage fit.
##' @author Ben B Hansen
##' @imports sandwich
##' 
estfun1for2 <- function(model, id.newdata, id.model=row.names(model.frame(model)), ...)
    {
        stopifnot(!missing(id.newdata))
        force(id.model)
        stopifnot(!is.null(id.model), !is.null(id.newdata))
        
        U <- sandwich:::estfun(model,...)
        Uplus1 <- rbind(U, rep(0, ncol(U)))
        Urows <- match(id.newdata, id.model, nomatch=nrow(Uplus1))
        Uplus1[Urows,]
    }
