context("Helper functions for building double-decker sandwich estimates of variance")

test_that("Bread and meat methods reproduce robustbase:::.vcov.avar1() as approp.", {
    expect_true(require('robustbase'))
    data(coleman)
     set.seed(0)
     m1 <- lmrob(Y ~ ., data=coleman) 
    expect_equal(m1$control$cov, ".vcov.avar1")
    expect_equivalent(robustbase:::.vcov.avar1(m1, m1$x), m1$cov)

    altavar1 <- function (obj, x = obj$x)  {
    stopifnot(is.list(ctrl <- obj$control))
    if (!is.null(ctrl$method) && !ctrl$method %in% c("SM", "MM")) 
        stop(".vcov.avar1() supports only SM or MM estimates")
    psi <- chi <- ctrl$psi
    if (is.null(psi)) 
        stop("parameter psi is not defined")
    stopifnot(is.numeric(c.chi <- ctrl$tuning.chi), is.numeric(c.psi <- ctrl$tuning.psi))
    r0 <- obj$init$resid
    r <- resid(obj)
    scale <- obj$scale
    if (is.null(x)) 
        x <- model.matrix(obj)
    bb <- 1/2
    n <- length(r)
    stopifnot(n == length(r0), is.matrix(x), n == nrow(x))
    p <- ncol(x)
    r.s <- r/scale
    r0.s <- r0/scale
    w <- Mpsi(r.s, cc = c.psi, psi = psi, deriv = 1)
    w0 <- Mchi(r0.s, cc = c.chi, psi = chi, deriv = 1)
    x.wx <- crossprod(x, x * w)
    if (inherits(A <- tryCatch(solve(x.wx) * scale, error = function(e) e), 
        "error")) {
        warning("X'WX is almost singular. Consider rather using cov = \".vcov.w\"")
        A <- tryCatch(solve(x.wx, tol = 0) * scale, error = function(e) e)
        if (inherits(A, "error")) 
            stop("X'WX is singular. Rather use cov = \".vcov.w\"")
    }
    a <- A %*% (crossprod(x, w * r.s)/mean(w0 * r0.s))
    w <- Mpsi(r.s, cc = c.psi, psi = psi)
    w0 <- Mchi(r0.s, cc = c.chi, psi = chi)
    Xww <- crossprod(x, w * w0)
    u1 <- A %*% crossprod(x, x * w^2) %*% (n * A)
    u2 <- a %*% crossprod(Xww, A)
    u3 <- A %*% tcrossprod(Xww, a)
    u4 <- mean(w0^2 - bb^2) * tcrossprod(a)
    list(cov=(u1 - u2 - u3 + u4)/n, A=A, a=a, w.estfun=w,r.s=r.s, w0.estfun=w0,bb=bb, Xw=x*w, n=n)
    }
    m1av1 <- altavar1(m1)
    expect_equivalent(with(m1av1, cbind(a, n*A) ), bread.lmrob(m1))
    expect_equivalent(with(m1av1, cbind(w0.estfun-mean(w0.estfun), Xw) ), estfun.lmrob(m1))
    expect_equivalent(with(m1av1, crossprod(cbind(w0.estfun-mean(w0.estfun), Xw) )/n), meat(m1))
    expect_false(isTRUE(all.equal( #this test demonstrates that centering the estimating function for scale
        with(m1av1, (w0.estfun-mean(w0.estfun)) *Xw) , #changes the off-diagonal pieces of the corresponding 
        with(m1av1, w0.estfun *Xw))))                  # column and row of the meat matrix
})


