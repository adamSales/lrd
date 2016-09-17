context("Helper functions for building double-decker sandwich estimates of variance")

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
