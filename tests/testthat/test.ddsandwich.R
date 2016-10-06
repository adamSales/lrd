context("Helper functions for building double-decker sandwich estimates of variance")

test_that("Our bread and meat methods reproduce robustbase:::.vcov.avar1()", {
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
    expect_equivalent(sandwich(m1), .vcov.avar2(m1)) #this fails, not sure why
})



test_that("predresid's predictions match those of `predict`",{
   ## from example(lm)
   ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
   trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
   group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
   weight <- c(ctl, trt)
   lm.D9 <- lm(weight ~ group)
   expect_equivalent(predresid(lm.D9, model.frame(lm.D9))$resid, resid(lm.D9))
   ## now from example(glm)
     counts <- c(18,17,15,20,10,20,25,13,12)
     outcome <- gl(3,1,9)
     treatment <- gl(3,3)
   glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
expect_equivalent(predresid(glm.D93, model.frame(glm.D93))$resid, resid(glm.D93, "resp"))
})

stopifnot(require('sandwich'))
test_that("predresid's gradient matrix matches reference calcs",{
   ## from example(lm)
   ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
   trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
   group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
   weight <- c(ctl, trt)
   lm.D9 <- lm(weight ~ group)
   ref1 <- sandwich:::estfun(lm.D9)
   pr1 <- predresid(lm.D9, model.frame(lm.D9))
   expect_equivalent(pr1$grad * resid(lm.D9), ref1)
   ## need a check on the glm version
})

test_that("predresid handles offsets appropriately",{
   ## now from example(glm)
   utils::data(anorexia, package = "MASS")     
   anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                   family = gaussian, data = anorexia)
   ##...
})

test_that("estfun1for2 doesn't break estfun",{
   ## from example(lm)
   ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
   trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
   group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
   weight <- c(ctl, trt)
   lm.D9 <- lm(weight ~ group)
   expect_equivalent(estfun1for2(lm.D9, row.names(model.frame(lm.D9))), sandwich::estfun(lm.D9))

   counts <- c(18,17,15,20,10,20,25,13,12)
   outcome <- gl(3,1,9)
   treatment <- gl(3,3)
   glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
   expect_equivalent(estfun1for2(glm.D93, row.names(model.frame(glm.D93))), sandwich::estfun(glm.D93))
})

test_that("estfun1for2 rearranges rows properly"{
   ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
   trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
   group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
   weight <- c(ctl, trt)
   lm.D9 <- lm(weight ~ group)
   ef0 <- sandwich:::estfun(lm.D9)
   ef0 <- rbind(rep(0, ncol(ef0)), ef0[2:1,], rep(0, ncol(ef0)))
   expect_equivalent(ef0, estfun1for2(lm.D9, as.character(c(23, 2, 1, 24))))
   expect_equivalent(ef0, estfun1for2(lm.D9, c(23, 2, 1, 24)))
})
