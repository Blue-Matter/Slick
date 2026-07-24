make_timeseries_slick <- function(nsim=12, nOM=2, nMP=2) {
  nHist <- 3
  nProj <- 4
  values <- array(
    seq_len(nsim * nOM * nMP * (nHist + nProj)),
    dim=c(nsim, nOM, nMP, 1, nHist + nProj)
  )
  timeseries <- Timeseries(
    Code="PI",
    Label="Performance indicator",
    Description="",
    Time=seq_len(nHist + nProj),
    TimeNow=nHist,
    TimeLab="Year",
    Value=values
  )
  slick <- Slick()
  Timeseries(slick) <- timeseries
  slick
}

test_that("plotTimeseries overplots ten randomly selected worms by default", {
  slick <- make_timeseries_slick()

  set.seed(123)
  plot <- plotTimeseries(slick, includeLabels=FALSE)
  worm_layers <- Filter(\(layer) "Sim" %in% names(layer$data), plot$layers)

  expect_length(worm_layers, 2)
  expect_equal(length(unique(worm_layers[[1]]$data$Sim)), 10)
  expect_equal(
    sort(unique(worm_layers[[1]]$data$Sim)),
    sort(unique(worm_layers[[2]]$data$Sim))
  )
})

test_that("worms can be disabled and are capped by available simulations", {
  slick <- make_timeseries_slick(nsim=3)

  no_worms <- plotTimeseries(slick, worms=0, includeLabels=FALSE)
  expect_length(Filter(\(layer) "Sim" %in% names(layer$data), no_worms$layers), 0)

  capped <- plotTimeseries(slick, worms=10, includeLabels=FALSE)
  worm_layers <- Filter(\(layer) "Sim" %in% names(layer$data), capped$layers)
  expect_equal(length(unique(worm_layers[[1]]$data$Sim)), 3)

  one_sim <- plotTimeseries(
    slick, sims=3, worms=10, includeLabels=FALSE
  )
  worm_layers <- Filter(\(layer) "Sim" %in% names(layer$data), one_sim$layers)
  expect_equal(unique(worm_layers[[1]]$data$Sim), 3)
})

test_that("worm trajectories retain OM and MP grouping variables", {
  slick <- make_timeseries_slick()

  plot <- plotTimeseries(
    slick, byOM=TRUE, byMP=TRUE, worms=4, includeLabels=FALSE
  )
  worm_layers <- Filter(\(layer) "Sim" %in% names(layer$data), plot$layers)

  expect_named(
    worm_layers[[2]]$data,
    c("Sim", "OM", "MP", "x", "y"),
    ignore.order=TRUE
  )
  expect_equal(length(unique(worm_layers[[2]]$data$Sim)), 4)
})

test_that("worms must be a non-negative integer", {
  slick <- make_timeseries_slick()

  expect_snapshot(error=TRUE, plotTimeseries(slick, worms=-1))
  expect_snapshot(error=TRUE, plotTimeseries(slick, worms=1.5))
})
