ecall = function(fun, pars, ctrl) {
  if (use_evaluate(ctrl)) {
    result = NULL
    now = proc.time()[[3L]]
    log = evaluate::evaluate(
      "result <- do.call(fun, pars)",
      stop_on_error = 1L,
      new_device = FALSE,
      include_timing = FALSE
    )
    elapsed = proc.time()[[3L]] - now
    log = Log$new(log)
  } else {
    now = proc.time()[[3L]]
    result = do.call(fun, pars)
    elapsed = proc.time()[[3L]] - now
    log = Log$new()
  }

  list(result = result, log = log, elapsed = elapsed)
}

train_worker = function(e, ctrl) {
  data = e$data
  learner = data$learner
  require_namespaces(learner$packages, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = data$task$clone(deep = TRUE)$filter(e$train_set)
  pars = c(list(task = task), learner$par_vals)

  if (ctrl$verbose)
    message(sprintf("Training learner '%s' on task '%s' ...", learner$id, task$id))
  res = ecall(learner$train, pars, ctrl)

  return(list(
    model = res$result,
    train_time = res$elapsed,
    train_log = res$log
  ))
}

predict_worker = function(e, ctrl) {
  data = e$data
  learner = data$learner
  require_namespaces(learner$packages, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = data$task$clone(deep = TRUE)$filter(e$test_set)
  pars = c(list(model = data$model, task = task), learner$par_vals)

  if (ctrl$verbose)
    message(sprintf("Predicting model of learner '%s' on task '%s' ...", learner$id, task$id))
  res = ecall(learner$predict, pars, ctrl)
  assert_class(res$result, "Prediction")

  return(list(
    prediction = res$result,
    predict_time = res$elapsed,
    predict_log = res$log
  ))
}

score_worker = function(e, ctrl) {
  data = e$data
  task = data$task
  measures = data$measures
  require_namespaces(unlist(lapply(measures, "[[", "packages")), "The following packages are required for the measures: %s")

  if (ctrl$verbose)
    message(sprintf("Scoring predictions of learner '%s' on task '%s' ...", data$learner$id, data$task$id))
  calc_all_measures = function() {
    setNames(lapply(measures, function(m) m$calculate(e)), ids(measures))
  }
  res = ecall(calc_all_measures, list(), ctrl)
  return(list(performance = res$result, score_time = res$elapsed))
}

experiment_worker = function(iteration, task, learner, resampling, measures, ctrl) {
  e = Experiment$new(task, learner, resampling = resampling, iteration = iteration, measures = measures)

  if (ctrl$verbose) {
    message(sprintf("Running learner '%s' on task '%s (iteration %i/%i)' ...", learner$id, task$id, iteration, resampling$iters))
    ctrl$verbose = FALSE
  }

  tmp = train_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  tmp = predict_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert(e$data, tmp)

  if (!ctrl$store_prediction)
    e$data["prediction"] = list(NULL)

  if (!ctrl$store_model)
    e$data["model"] = list(NULL)

  remove(e$data, c("task", "learner", "resampling", "measures"))
}
