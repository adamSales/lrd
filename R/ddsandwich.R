##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Bread method for lmrob objects
##' @param x an lmrob object produced using an MM/SM estimator chain
##' @param ... 
##' @return k by (k+1) matrix, with first column for scale estimate and rows, remaining cols for coefficients
##' 
##' @author Ben B Hansen
bread.lmrob <- function(x, ...)
{
  stopifnot(is.list(ctrl <- x$control))
  if (!(!is.null(ctrl$method) && nchar(ctrl$method)<=2 &&
      substr(ctrl$method, nchar(ctrl$method),nchar(ctrl$method))=="M") )
      stop("bread.lmrob() supports only SM or MM estimates")

       psi <- chi <- ctrl$psi
    if (is.null(psi)) 
        stop("parameter psi is not defined")
    stopifnot(is.numeric(c.chi <- ctrl$tuning.chi), is.numeric(c.psi <- ctrl$tuning.psi))
    r0 <- x$init$resid
    r <- resid(x)
    scale <- x$scale
    xmat <- model.matrix(x)
       bb <- 1/2
    n <- length(r)
    stopifnot(n == length(r0), is.matrix(xmat), n == nrow(xmat))
    p <- ncol(xmat)
    r.s <- r/scale
    r0.s <- r0/scale
    w <- robustbase::Mpsi(r.s, cc = c.psi, psi = psi, deriv = 1)
    w0 <- robustbase::Mchi(r0.s, cc = c.chi, psi = chi, deriv = 1)
    x.wx <- crossprod(xmat, xmat * w)
    if (inherits(A <- tryCatch(solve(x.wx) * scale, error = function(e) e), 
        "error")) {
        A <- tryCatch(solve(x.wx, tol = 0) * scale, error = function(e) e)
        if (inherits(A, "error")) 
            { stop("X'WX is singular.") } else warning("X'WX is almost singular.")
    }
    ## At this point A has no sample size scaling, as in robustbase:::.vcov.avar1
    ## The lack of scaling there precisely compensates for the lack of scaling of the crossproduct
    a <- A %*% (crossprod(xmat, w * r.s)/mean(w0 * r0.s))
    colnames(a) <- "sigma"
    ## Now we restore sample size scaling to A
    A <- n * A
    
    cbind(a, A)
}

estfun.lmrob <- function(x, ...)
{
  stopifnot(is.list(ctrl <- x$control))
  if (!(!is.null(ctrl$method) && nchar(ctrl$method)<=2 &&
      substr(ctrl$method, nchar(ctrl$method),nchar(ctrl$method))=="M") )
      stop("estfun.lmrob() supports only SM or MM estimates")

  xmat <- model.matrix(x)
    xmat <- naresid(x$na.action, xmat)
       psi <- chi <- ctrl$psi
    if (is.null(psi)) 
        stop("parameter psi is not defined")
    stopifnot(is.numeric(c.chi <- ctrl$tuning.chi), is.numeric(c.psi <- ctrl$tuning.psi))
    r0 <- x$init$resid
    r <- resid(x)
    scale <- x$scale

       bb <- 1/2
    n <- length(r)
    stopifnot(n == length(r0), is.matrix(xmat), n == nrow(xmat))
    p <- ncol(xmat)
    r0.s <- r0/scale
    w0 <- robustbase::Mchi(r0.s, cc = c.chi, psi = chi, deriv = 1)
    Usigma <- w0 - bb

    r.s <- r/scale
    w <- robustbase::Mpsi(r.s, cc = c.psi, psi = psi, deriv = 1)
    Ubeta <- w * xmat
    rval <- cbind("sigma"=Usigma, Ubeta)
       attr(rval, "assign") <- NULL
    attr(rval, "contrasts") <- NULL
    rval
    }


sandwich <- function (x, bread. = sandwich::bread, meat. = sandwich::meat, ...) 
{
    if (is.list(x) && !is.null(x$na.action)) 
        class(x$na.action) <- "omit"
    if (is.function(bread.)) 
        bread. <- bread.(x)
    if (is.function(meat.)) 
        meat. <- meat.(x, ...)
    n <- NROW(sandwich::estfun(x))
    ## the t() in the below is the only difference from sandwich::sandwich()
    return(1/n * (bread. %*% meat. %*% t(bread.)))
}


##' Differences of dependent variables and model predictions on a new data set
##'
##' the prediction residuals themselves will be variables that balance tests are applied to.
##' gradients of the prediction residuals will be used to build rows of the A21 matrix
##' in a double-decker sandwich.  The same double decker has another role for cross products of the first stage estimating
##' function and the second stage estimating function. Data points that contribute only to one or the other of these
##' make not contribution to this cross product, and can be ignored; thus the role for the partial mapping of
##' rows of one data frame into the other. 
##'
##' DEPRACATED. After writing this, I decided to pursue a different approach. 
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
##' DEPRACATED. After writing this, I decided to pursue a different approach.
##' 
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
