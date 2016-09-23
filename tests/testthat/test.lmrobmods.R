context("Non-standard uses of robustbase's lmrob and helpers")

test_that("On-label use of .vcov.avar1()",{
    expect_true(require('robustbase'))
    data(coleman)
     set.seed(0)
     m1 <- lmrob(Y ~ ., data=coleman) 
     expect_equivalent(robustbase:::.vcov.avar1(m1, m1$x), m1$cov)
     initFun1 <- function(x, y, control, mf) {
         init.S <- lmrob.S(x, y, control)
         list(coefficients=init.S$coef,
              scale = init.S$scale, #this winds up being passed unchanged through lmrob.fit/lmrob
              residuals = init.S$resid)
     }
     set.seed(0)
    m2 <- lmrob(Y ~ ., data=coleman, method = "M", init = initFun1)
    expect_false( all.equal(m1$cov, # the two cov estimation methods could give the same answer just 
                            m2$cov, # by chance; confirming it's not happening in this example
                            check.attributes = FALSE) )
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
    
          })
