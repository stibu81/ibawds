library(vdiffr)

test_that("test density_plot()", {
  expect_doppelganger("density_plot normal",
    density_plot(dnorm, c(-5, 7), mean = 1, sd = 2, to = 3)
  )
  expect_doppelganger("density_plot chi2",
    density_plot(dchisq, c(-1, 10), df = 3, from = 2, points = c(1, 6))
  )
  expect_doppelganger("density_plot weibull",
    density_plot(dweibull, c(-2, 7), shape = 1, from = 1, to = 3.2,
                 var = "k", title = "Weibull")
  )
})


test_that("test distribution_plot()", {
  expect_doppelganger("distribution_plot normal",
    distribution_plot(pnorm, c(-5, 7), mean = 1, sd = 2, points = c(-1, 4))
  )
  expect_doppelganger("distribution_plot uniform",
    distribution_plot(punif, c(-2, 7), min = 1, max = 5, points = c(2, 4),
                      var = "t", title = "Uniform")
  )
})
