context("parseScenario")

test_that("parseASScenario1", {
  expect_true(inherits(testscenario1, "ASScenario"))

  expect_equal(dim(testscenario1$algo.runs), c(6840, 5))
  expect_equal(dim(testscenario1$algo.runstatus), c(1368, 7))
  expect_equal(dim(testscenario1$cv.splits), c(1368, 3))
  expect_equal(dim(testscenario1$feature.costs), NULL)
  expect_equal(dim(testscenario1$feature.runstatus), c(1368, 3))
  expect_equal(dim(testscenario1$feature.values), c(1368, 48))

  expect_equal(length(testscenario1$desc$features_deterministic), 46)
  expect_equal(length(testscenario1$desc$feature_steps), 1)
  nams = names(testscenario1$desc$feature_steps)
  expect_equal(length(testscenario1$desc$feature_steps[[nams[1]]]$provides), 46)
  expect_equal(testscenario1$desc$algorithm_cutoff_memory, 16384)
  expect_equal(testscenario1$desc$algorithm_cutoff_time, 3600)
  expect_equal(names(testscenario1$desc$metainfo_algorithms), c("X2clsQ", "QuBE", "quantor", "sKizzo", "sSolve"))
  expect_equal(testscenario1$desc$default_steps, "all_feats")
  expect_equal(testscenario1$desc$features_cutoff_memory, as.numeric(NA))
  expect_equal(testscenario1$desc$features_cutoff_time, as.numeric(NA))
  expect_equal(testscenario1$desc$features_stochastic, character(0))
  val = FALSE
  names(val) = "runtime"
  expect_equal(testscenario1$desc$maximize, val)
  expect_equal(testscenario1$desc$number_of_feature_steps, 1)
  expect_equal(testscenario1$desc$performance_measures, "runtime")
  val = "runtime"
  names(val) = "runtime"
  expect_equal(testscenario1$desc$performance_type, val)
  expect_equal(testscenario1$desc$scenario_id, "QBF-2011")
})

test_that("parseASScenario2", {
  expect_true(inherits(testscenario2, "ASScenario"))

  expect_equal(dim(testscenario2$algo.runs), c(36177, 5))
  expect_equal(dim(testscenario2$algo.runstatus), c(1167, 33))
  expect_equal(dim(testscenario2$cv.splits), c(1167, 3))
  expect_equal(dim(testscenario2$feature.costs), c(1167, 12))
  expect_equal(dim(testscenario2$feature.runstatus), c(1167, 12))
  expect_equal(dim(testscenario2$feature.values), c(1167, 117))

  expect_equal(length(testscenario2$desc$features_deterministic), 115)
  expect_equal(length(testscenario2$desc$feature_steps), 10)
  nams = names(testscenario2$desc$feature_steps)
  expect_equal(length(testscenario2$desc$feature_steps[[nams[1]]]$provides), 14)
  expect_equal(testscenario2$desc$feature_steps[[nams[1]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[2]]]$provides), 10)
  expect_equal(testscenario2$desc$feature_steps[[nams[2]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[3]]]$provides), 5)
  expect_equal(testscenario2$desc$feature_steps[[nams[3]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[4]]]$provides), 20)
  expect_equal(testscenario2$desc$feature_steps[[nams[4]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[5]]]$provides), 6)
  expect_equal(length(testscenario2$desc$feature_steps[[nams[6]]]$provides), 18)
  expect_equal(testscenario2$desc$feature_steps[[nams[6]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[7]]]$provides), 2)
  expect_equal(testscenario2$desc$feature_steps[[nams[7]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[8]]]$provides), 11)
  expect_equal(testscenario2$desc$feature_steps[[nams[8]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[9]]]$provides), 11)
  expect_equal(testscenario2$desc$feature_steps[[nams[9]]]$requires, "Pre")
  expect_equal(length(testscenario2$desc$feature_steps[[nams[10]]]$provides), 18)
  expect_equal(testscenario2$desc$feature_steps[[nams[10]]]$requires, "Pre")
  expect_equal(testscenario2$desc$algorithm_cutoff_memory, as.numeric(NA))
  expect_equal(testscenario2$desc$algorithm_cutoff_time, 1200)
  expect_equal(length(names(testscenario2$desc$metainfo_algorithms)), 31)
  expect_equal(testscenario2$desc$default_steps, c("Pre", "Basic", "KLB", "CG"))
  expect_equal(testscenario2$desc$features_cutoff_memory, as.numeric(NA))
  expect_equal(testscenario2$desc$features_cutoff_time, 1200)
  expect_equal(testscenario2$desc$features_stochastic, character(0))
  val = FALSE
  names(val) = "runtime"
  expect_equal(testscenario2$desc$maximize, val)
  expect_equal(testscenario2$desc$number_of_feature_steps, 10)
  expect_equal(testscenario2$desc$performance_measures, "runtime")
  val = "runtime"
  names(val) = "runtime"
  expect_equal(testscenario2$desc$performance_type, val)
  expect_equal(testscenario2$desc$scenario_id, "SAT12-INDU")
})

test_that("parseASScenario checks feature runstatus levels", {
  expect_error(parseASScenario(file.path("broken-status-levels")),
    "Feature runstatus file contains illegal levels:")
})

test_that("parseASScenario verifies that all instances appear in CV", {
  expect_error(parseASScenario(file.path("broken-cv"), "Fold allocations given for"))
})
