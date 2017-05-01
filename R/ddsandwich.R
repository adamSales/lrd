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

    n <- length(r)
    stopifnot(n == length(r0), is.matrix(xmat), n == nrow(xmat))
    p <- ncol(xmat)
    r0.s <- r0/scale
    w0 <- robustbase::Mchi(r0.s, cc = c.chi, psi = chi)
    Usigma <- scale(w0, center=TRUE, scale=FALSE)

    r.s <- r/scale
    w <- robustbase::Mpsi(r.s, cc = c.psi, psi = psi)

  Ubeta <- w * xmat
    rval <- cbind("sigma"=Usigma, Ubeta)
       attr(rval, "assign") <- NULL
    attr(rval, "contrasts") <- NULL
    rval
    }

##' Overloading of sandwich::sandwich to accommodate non-square bread
##'
##' The sandwich package's sandwich function presumes the bread matrix
##' to be symmetric. Obviously this won't do if the bread is rectangular but not
##' square.  To accommodate that scenario, this calculates 
##' @title 
##' @param x a fitted model object, as in sandwich::sandwich
##' @param bread. function or matrix, 
##' @param meat. function or matrix, as in sandwich::sandwich
##' @param ... additional arguments to downstream methods, as in sandwich::sandwich
##' @return matrix, bread %*% meat %*% t(bread)
##' @author Ben Hansen
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


.vcov.avar2 <- function(obj, x=obj$x, posdef.meth = c("posdefify","orig"))
{ ## was .vcov.MM
    stopifnot(is.list(ctrl <- obj$control))
    ## works only for MM & SM estimates:
###    if (!is.null(ctrl$method) && !ctrl$method %in% c('SM', 'MM'))
### I replaced line above w/ 2 lines below for own reasons unrelated to bug fix -BH
    if (!is.null(ctrl$method) && !(nchar(ctrl$method)==2 &&
      substr(ctrl$method, nchar(ctrl$method),nchar(ctrl$method))=="M") )
    stop('.vcov.avar2() supports only SM or MM estimates')
    ## set psi and chi constants
    psi <- chi <- ctrl$psi
    if (is.null(psi)) stop('parameter psi is not defined')
    stopifnot(is.numeric(c.chi <- ctrl$tuning.chi),
	      is.numeric(c.psi <- ctrl$tuning.psi))

    ## need (r0, r, scale, x, c.psi,c.chi, bb)
    r0 <- obj$init$resid
    r <- resid(obj)
    scale <- obj$scale
    if (is.null(x))  x <- model.matrix(obj)
    bb <- 1/2 ## this is always 1/2 for S estimates by convention
### --- start code from .vcov.MM ---
    ## scaled residuals
    n <- length(r)
    stopifnot(n == length(r0), is.matrix(x), n == nrow(x))
    p <- ncol(x)
    ## Next 2 lines added post-.vcov.MM, addressing #6471.
    ## This assumes initial S-estimate solved sum( loss )/(n-p) == bb,
    ## not mean( loss ) == bb as assumed in .vcov.avar1
    adj <- (n-p)/n
    bb <- bb * adj
    r.s	 <- r / scale # final   scaled residuals
    r0.s <- r0 / scale # initial scaled residuals
    w  <- Mpsi(r.s, cc = c.psi, psi = psi, deriv = 1)
    w0 <- Mchi(r0.s, cc = c.chi, psi = chi, deriv = 1)
    ## FIXME for multivariate y :
    x.wx <- crossprod(x, x * w)
    if(inherits(A <- tryCatch(solve(x.wx) * scale,
			      error=function(e)e), "error")) {
	warning("X'WX is almost singular. Consider rather using cov = \".vcov.w\"")
	A <- tryCatch(solve(x.wx, tol = 0) * scale, error=function(e)e)
	if(inherits(A, "error"))
	    stop("X'WX is singular. Rather use cov = \".vcov.w\"")
    }
    a <- A %*% (crossprod(x, w * r.s) / mean(w0 * r0.s))
    w <- Mpsi( r.s, cc = c.psi, psi = psi)

    ## 3) now the standard part  (w, x, r0.s,  n, A,a, c.chi, bb)
    w0 <- Mchi(r0.s, cc = c.chi, psi = chi)
    Xww <- crossprod(x, w*w0)
    u1 <- A %*% crossprod(x, x * w^2) %*% (n * A)
    u2 <- a %*% crossprod(Xww, A)
    u3 <- A %*% tcrossprod(Xww, a)
    u4 <- mean(w0^2 - bb^2) * tcrossprod(a)

    ## list(cov = matrix((u1 - u2 - u3 + u4)/n, p, p),
    ##      wt = w / r.s, a = a)
### --- end code from .vcov.MM ---
    ret <- (u1 - u2 - u3 + u4)/n

    ## this might not be a positive definite matrix
    ## check eigenvalues (symmetric: ensure non-complex)
    ev <- eigen(ret, symmetric = TRUE)
    if (any(neg.ev <- ev$values < 0)) { ## there's a problem
	posdef.meth <- match.arg(posdef.meth)
	if(ctrl$trace.lev)
	    message("fixing ", sum(neg.ev),
		    " negative eigen([",p,"])values")
	Q <- ev$vectors
	switch(posdef.meth,
	       "orig" = {
		   ## remove negative eigenvalue:
		   ## transform covariance matrix into eigenbasis
		   levinv <- solve(Q)
		   cov.eb <- levinv %*% ret %*% Q
		   ## set vectors corresponding to negative ev to zero
		   cov.eb[, neg.ev] <- 0
		   ## cov.eb[cov.eb < 1e-16] <- 0
		   ## and transform back
		   ret <- Q %*% cov.eb %*% levinv
	       },
	       "posdefify" = {
		   ## Instead of using	require("sfsmisc") and
		   ## ret <- posdefify(ret, "someEVadd",eigen.m = ev,eps.ev = 0)
		   lam <- ev$values
		   lam[neg.ev] <- 0
		   o.diag <- diag(ret)# original one - for rescaling
		   ret <- Q %*% (lam * t(Q)) ## == Q %*% diag(lam) %*% t(Q)
		   ## rescale to the original diagonal values
		   ## D <- sqrt(o.diag/diag(ret))
		   ## where they are >= 0 :
		   D <- sqrt(pmax.int(0, o.diag)/diag(ret))
		   ret[] <- D * ret * rep(D, each = p) ## == diag(D) %*% m %*% diag(D)
	       },
	       stop("invalid 'posdef.meth': ", posdef.meth))
    }
    attr(ret,"weights") <- w / r.s
    attr(ret,"eigen") <- ev
    ret
}## end{.vcov.avar2}

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
