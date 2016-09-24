context("Non-standard uses of robustbase's lmrob and helpers")

test_that("On-label use of .vcov.avar1()",{
    expect_true(require('robustbase'))
    data(coleman)
     set.seed(0)
     m1 <- lmrob(Y ~ ., data=coleman) 
    expect_equal(m1$control$cov, ".vcov.avar1")
    expect_equivalent(robustbase:::.vcov.avar1(m1, m1$x), m1$cov)
     initFun1 <- function(x, y, control, mf) {
         init.S <- lmrob.S(x, y, control)
         list(coefficients=init.S$coef,
              scale = init.S$scale, #this winds up being passed unchanged through lmrob.fit/lmrob
              residuals = init.S$resid)
     }
     set.seed(0)
    m2 <- lmrob(Y ~ ., data=coleman, method = "M", init = initFun1)
    expect_equal(m2$control$cov, ".vcov.w")
    expect_false( isTRUE(all.equal(m1$cov, # the two cov estimation methods could give the same answer just 
                            m2$cov, # by chance; confirming it's not happening in this example
                            check.attributes = FALSE)) )
     expect_error(robustbase:::.vcov.avar1(m2, m2$x), "supports only SM or MM estimates")
     m3 <- m2
     m3$control$method <- "SM" # since it really is an SM estimate
    expect_equivalent(robustbase:::.vcov.avar1(m3, m3$x), m1$cov)
    m4 <- lmrob(Y ~ ., data=coleman, method = "M", init = m2$init)
    expect_error(robustbase:::.vcov.avar1(m4, m4$x), "supports only SM or MM estimates")
    m5 <- m4
    m5$control$method <- "SM"
    expect_equivalent(robustbase:::.vcov.avar1(m5, m5$x), m1$cov)
   })

test_that("Off-label use of .vcov.avar1()",{
    expect_true(require('robustbase'))
    data(coleman)
     set.seed(0)
     m1 <- lmrob(Y ~ . + I(sstatus>0), data=coleman) 
     expect_equivalent(robustbase:::.vcov.avar1(m1, m1$x), m1$cov)
     initFun2 <- function(x, y, control, mf) {
         init.S <- lmrob.S(x, y, control)
         list(coefficients=numeric(length(init.S$coef)), # lmrob.fit check that this is present, has an entry for each col of x
              scale = init.S$scale, 
              residuals = init.S$resid)
     }
    set.seed(0)
    m2 <- lmrob(Y ~ .  + I(sstatus>0), data=coleman, method = "M", init = initFun2) # works, but...
    expect_equal(coef(m2), rep(0, length(coef(m2)))) # oops.
     initFun3 <- function(x, y, control, mf) {
         xtrunc <- x[,-ncol(x)]
         init.S <- lmrob.S(xtrunc, y, control)
         list(coefficients=c(init.S$coef, 0), 
              scale = init.S$scale, 
              residuals = init.S$resid)
     }
    m3 <- lmrob(Y ~ .  + I(sstatus>0), data=coleman, method = "M", init = initFun3)
    ## The answer is in same ballpark as what you get w/ unrestricted `I(sstatus>0)` coef:
    expect_false(isTRUE(all.equal(coef(m1), coef(m3), tol=.01))) 
    expect_equal(coef(m1), coef(m3), tol=.02)
    m3$control$method <- "SM"
    expect_false(all.equal(m1$cov, robustbase:::.vcov.avar1(m3), check.attributes=F))
    expect_equivalent(m1$cov, robustbase:::.vcov.avar1(m3), tol=.1)

    ## this initfun variant is the same as initfun3, except it zeroes out
    ## coeff's corresponding to the first term in the regression formula
    initFun4 <- function(x, y, control, mf) {
        stopifnot(!is.null(attr(x, "assign")))
        stopifnot(any( zerocols <- (attr(x, "assign") == 1L) ))
        xtrunc <- x[, !zerocols]
        init.S <- lmrob.S(xtrunc, y, control)
        thecoefs <- numeric(ncol(x))
        thecoefs[!zerocols] <- init.S$coef
        list(coefficients=thecoefs, 
              scale = init.S$scale, 
              residuals = init.S$resid)
     }
    m4 <- lmrob(Y ~ I(sstatus>0) + ., data=coleman, method = "M", init = initFun4)
    expect_equal(coef(m3), coef(m4)[names(coef(m3))], tol=.01)
  
})

test_that("Modified lmrob.S does what it should",{
    expect_true(require('robustbase'))
    data(coleman)
     set.seed(0)
    m5 <- lmrob(Y ~ I(sstatus>0) + ., data=coleman, method = "M", init = lmrob.S.mod)
    initFun4 <- function(x, y, control, mf) {
        stopifnot(!is.null(attr(x, "assign")))
        stopifnot(any( zerocols <- (attr(x, "assign") == 1L) ))
        xtrunc <- x[, !zerocols, drop=FALSE]
        init.S <- lmrob.S(xtrunc, y, control)
        thecoefs <- numeric(ncol(x))
        thecoefs[!zerocols] <- init.S$coef
        list(coefficients=thecoefs, 
              scale = init.S$scale, 
              residuals = init.S$resid)
    }
         set.seed(0)
    m4 <- lmrob(Y ~ I(sstatus>0) + ., data=coleman, method = "M", init = initFun4)
    expect_equivalent(coef(m4), coef(m5))
    expect_equivalent(vcov(m4), vcov(m5))
    m5$control$method <- m4$control$method <- "SM"    
    expect_equivalent(robustbase:::.vcov.avar1(m4), robustbase:::.vcov.avar1(m5))
})
