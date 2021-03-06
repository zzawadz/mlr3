#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark of the cross-product of learners, tasks, and resampling strategies (possibly in parallel).
#'
#'
#' @param tasks (`list` of [Task])\cr
#'   List of objects of type [Task].
#' @param learners (`list` of [Learner])\cr
#'   List of objects of type [Learner].
#' @param resamplings (`list` of [Resampling])\cr
#'   List of objects of type [Resampling].
#' @param measures (list of [Measure])\cr
#'   List of performance measures used to assess the predictive performance.
#'   Defaults to the respective measures stored in `task`.
#' @param ctrl (list returned by [exec_control()])\cr
#'   Object to control various parts of the execution.
#'   See [exec_control()].
#' @return [BenchmarkResult].
#' @export
#' @examples
#' tasks = mlr_tasks$mget(c("iris", "sonar"))
#' learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
#' resamplings = mlr_resamplings$mget(c("holdout", "cv"))
#' measures = mlr_measures$mget(c("acc", "time_train"))
#' bmr = benchmark(tasks, learners, resamplings, measures)
#' bmr$performance
#'
#' # Overview of of resamplings that were conducted internally
#' rrs = bmr$resample_results
#' print(rrs)
#'
#' # Extract first ResampleResult
#' rr = bmr$resample_result(hash = rrs$hash[1])
#' print(rr)
#'
#' # Extract predictions of first experiment of this resampling
#' rr$experiment(1)$prediction
benchmark = function(tasks, learners, resamplings, measures = NULL, ctrl = exec_control()) {
  assert_list(tasks, "Task", min.len = 1L)
  assert_list(learners, "Learner", min.len = 1L)
  assert_list(resamplings, "Resampling", min.len = 1L)

  if (is.null(measures)) {
    measures = unname(lapply(tasks, "[[", "measures"))
  } else {
    assert_list(measures, "Measure", min.len = 1L)
    measures = replicate(length(tasks), measures, simplify = FALSE)
  }
  # TODO: call assert measures on grid

  # Instantiate resampling for each task
  grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))
  instances = .mapply(function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]), grid, list())
  grid$instance = seq_row(grid)

  # some more validity checks
  assert_unique_hashes(tasks)
  assert_unique_hashes(learners)
  assert_unique_hashes(instances)

  # Cross join task x learner combinations
  tmp = CJ(task = seq_along(tasks), learner = seq_along(learners))
  grid = grid[tmp, on = "task", allow.cartesian = TRUE]

  # Cross join resampling iterations
  tmp = rbindlist(lapply(seq_along(instances), function(i) data.table(instance = i, iter = seq_len(instances[[i]]$iters))))
  grid = grid[tmp, on = "instance", allow.cartesian = TRUE]

  # compute hashes
  task = learner = instance = NULL
  grid[, "hash" := hash.list(list(task = tasks[[task]], learner = learners[[learner]], resampling = instances[[instance]])), by = c("task", "learner", "instance")]

  if (use_future(ctrl)) {
    debug("Running resample() via future with %i iterations", nrow(grid))

    # randomize order for parallelization
    grid = grid[sample.int(nrow(grid))]

    tmp = future.apply::future_mapply(experiment_worker,
      task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance], iteration = grid$iter, measures = measures[grid$task],
      MoreArgs = list(ctrl = ctrl), SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.packages = "mlr3"
      )
  } else {
    debug("Running benchmark() sequentially with %i iterations", nrow(grid))
    tmp = mapply(experiment_worker,
      task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance], iteration = grid$iter, measures = measures[grid$task],
      MoreArgs = list(ctrl = ctrl), SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  }

  res = data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance], measures = measures[grid$task], hash = grid$hash)
  res = rcbind(res, combine_experiments(tmp))

  BenchmarkResult$new(res)
}
