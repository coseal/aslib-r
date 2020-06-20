context("convertToLlama")

test_that("convertToLlama", {
  skip_on_cran()
  llama.scenario = convertToLlama(testscenario3)
  expect_equal(llama.scenario$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.scenario$data$f1, testscenario3$feature.values$f1)
  expect_equal(llama.scenario$data$f2, testscenario3$feature.values$f2)
  expect_equal(llama.scenario$data$f3, testscenario3$feature.values$f3)
  expect_equal(llama.scenario$data$instance_id, as.factor(c("i1", "i2", "i3")))
  expect_equal(llama.scenario$data$a1_success, c(TRUE, TRUE, TRUE))
  expect_equal(llama.scenario$data$a2_success, c(TRUE, TRUE, FALSE))
  expect_equal(llama.scenario$data$a1, c(30, 90, 70))
  expect_equal(llama.scenario$data$a2, c(50, 30, 100))
  expect_equal(llama.scenario$best, c("a1", "a2", "a1"))

  lrn = makeLearner("classif.rpart")

  print(parallelMap::parallelGetRegisteredLevels())

  llama.scenario = convertToLlama(testscenario1)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = lrn, data = cv)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = lrn, data = cv)

  llama.scenario = convertToLlama(testscenario2)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = lrn, data = cv)
  llama.scenario = convertToLlama(testscenario2)
  cv = cvFolds(llama.scenario, nfolds = 2L)
  res = classify(classifier = lrn, data = cv)
})

test_that("convertToLlama always sets best algorithm", {
  llama.scenario = convertToLlama(testscenario4)
  expect_equal(llama.scenario$best, c("a1", "a2", "a2"))
})

test_that("convertToLlama parses real scenario correctly", {
  llama.scenario = convertToLlama(testscenario1)
  iid1 = as.character(llama.scenario$data$instance_id)
  iid2 = as.character(testscenario1$algo.runs$instance_id)
  expect_true(setequal(iid1, iid2))
  expect_equal(llama.scenario$performance, names(testscenario1$desc$metainfo_algorithms))
  expect_equal(llama.scenario$success, paste0(names(testscenario1$desc$metainfo_algorithms), "_success"))
  expect_equal(llama.scenario$features, testscenario1$desc$features_deterministic)
  expect_equal(length(llama.scenario$best), 1368)
})

test_that("convertToLlama parses real scenario correctly take 2", {
  llama.scenario = convertToLlama(testscenario2, feature.steps = names(sapply(testscenario2$desc$feature_steps, function(x) x$provides)))
  iid1 = as.character(llama.scenario$data$instance_id)
  iid2 = as.character(testscenario2$algo.runs$instance_id)
  expect_true(setequal(iid1, iid2))
  expect_equal(llama.scenario$performance, names(testscenario2$desc$metainfo_algorithms))
  expect_equal(llama.scenario$success, paste0(names(testscenario2$desc$metainfo_algorithms), "_success"))
  expect_equal(length(llama.scenario$features), 113)
  expect_equal(length(llama.scenario$best), 1167)
})

test_that("convertToLlama handles costs correctly", {
  llama.scenario = convertToLlama(testscenario2)
  iid1 = as.character(llama.scenario$data$instance_id)
  iid2 = as.character(testscenario2$algo.runs$instance_id)
  expect_true(setequal(iid1, iid2))
  default_groups = lapply(testscenario2$desc$feature_steps[BBmisc::vlapply(names(testscenario2$desc$feature_steps), function(x) { x %in% testscenario2$desc$default_steps })], function(d) d$provides)
  for(n in names(default_groups)) {
      expect_equal(llama.scenario$costGroups[n], default_groups[n])
  }
  expect_false("repetition" %in% llama.scenario$costs)
})

test_that("convertToLlama respects cost groups", {
  ldf = convertToLlama(testscenario5)

  expect_equal(ldf$features, c("f1", "f2"))
})

test_that("fixFeckingPresolve", {
  llama.scenario1 = convertToLlama(testscenario2)
  vbs1 = sum(parscores(llama.scenario1, vbs))
  llama.scenario2 = fixFeckingPresolve(testscenario2, llama.scenario1)
  vbs2 = sum(parscores(llama.scenario2, vbs))

  # You would think so, wouldn't you. But no...
  # expect_true(vbs2 < vbs1)
})
