## deviceConfig.R
## -----------------------------------------------------------------------------
## Runtime device detection + placement policy for the modernized (keras3) GPU
## ML wrapper. Sourced by gpuMachineLearning.R.
##
## Goal: one code path that runs on Apple Metal (primary) or CUDA (when an
## NVIDIA GPU is present), falling back to CPU. "CUDA if available" is achieved
## by keras3/TensorFlow placing ops automatically once the right backend build
## is installed; this file only adds the policy glue (RNN-on-Metal caution,
## multi-GPU gating, XGBoost device params).
##
## Sourcing this file has NO side effects beyond defining functions/options.
## Nothing here requires TensorFlow to be installed (all probes are guarded), so
## it is safe to source even before the Python env is set up.
## -----------------------------------------------------------------------------

## --- OS / architecture helpers ---------------------------------------------

.dc_is_darwin <- function() identical(Sys.info()[["sysname"]], "Darwin")

.dc_is_apple_silicon <- function() {
  .dc_is_darwin() && (Sys.info()[["machine"]] %in% c("arm64", "aarch64"))
}

## --- Device detection -------------------------------------------------------
## Returns one of: "cuda", "metal", "cpu".
## Result is cached (TF probing is not free); pass refresh = TRUE to re-detect.

.dc_cache <- new.env(parent = emptyenv())

detect_device <- function(refresh = FALSE, verbose = FALSE) {
  if (!refresh && !is.null(.dc_cache$device)) return(.dc_cache$device)

  dev <- tryCatch({
    if (!requireNamespace("tensorflow", quietly = TRUE)) {
      ## No TF backend installed yet: we cannot GPU-train NNs. Report cpu, but
      ## note the arch so callers/logs can hint that Metal is *expected* here.
      if (.dc_is_apple_silicon() && verbose)
        message("[deviceConfig] tensorflow not installed; NN device = cpu ",
                "(Apple Silicon detected — install keras3 + tensorflow-metal to enable Metal).")
      "cpu"
    } else {
      tf <- tensorflow::tf
      gpus <- tryCatch(tf$config$list_physical_devices("GPU"), error = function(e) list())
      has_gpu <- length(gpus) > 0
      is_cuda_build <- tryCatch(isTRUE(tf$test$is_built_with_cuda()), error = function(e) FALSE)
      if (has_gpu && is_cuda_build) "cuda"
      else if (has_gpu && .dc_is_apple_silicon()) "metal"
      else if (has_gpu) "cuda"     # GPU present, non-Apple, non-CUDA build is unusual; treat as cuda-like
      else "cpu"
    }
  }, error = function(e) "cpu")

  .dc_cache$device <- dev
  if (verbose) message(sprintf("[deviceConfig] detected NN device: %s", dev))
  dev
}

## --- One-time backend configuration ----------------------------------------
## Call once after loading keras3/tensorflow, before building models.
## - CUDA: enable per-GPU memory growth (avoids grabbing all VRAM up front).
## - Metal: nothing to configure (single unified-memory GPU); mixed precision is
##   left OFF by default (fp16 on Metal is unreliable — opt in explicitly).

configure_device <- function(device = detect_device(), mixed_precision = FALSE, verbose = TRUE) {
  ok <- tryCatch({
    if (!requireNamespace("tensorflow", quietly = TRUE)) return(invisible(device))
    tf <- tensorflow::tf
    gpus <- tryCatch(tf$config$list_physical_devices("GPU"), error = function(e) list())

    if (device == "cuda" && length(gpus) > 0) {
      for (g in gpus) tryCatch(tf$config$experimental$set_memory_growth(g, TRUE),
                               error = function(e) NULL)
    }
    if (mixed_precision) {
      ## Only enable where it's reliable. fp16 on Metal is known-unreliable, so
      ## we do NOT flip the policy there even when asked — validate first, then
      ## opt in explicitly via keras3::keras$mixed_precision$set_global_policy().
      if (device == "cuda") {
        tryCatch(keras3::keras$mixed_precision$set_global_policy("mixed_float16"),
                 error = function(e) NULL)
      } else if (verbose) {
        message("[deviceConfig] mixed_precision requested but skipped on device '",
                device, "' (fp16 unreliable off-CUDA); set the policy manually if validated.")
      }
    }
    TRUE
  }, error = function(e) FALSE)

  if (verbose) {
    n <- tryCatch(length(tensorflow::tf$config$list_physical_devices("GPU")), error = function(e) 0L)
    message(sprintf("[deviceConfig] device=%s  visible_GPUs=%d  mixed_precision=%s  configured=%s",
                    device, n, mixed_precision, ok))
  }
  invisible(device)
}

## --- RNN placement policy ---------------------------------------------------
## Phase 2 finding (TF 2.19.1 + tensorflow-metal 1.2.0, this hardware): recurrent
## layers are NUMERICALLY CORRECT on Metal (stateful biGRU, biLSTM all match CPU
## to ~6e-8) — the historical Metal RNN accuracy bug does NOT reproduce here. The
## real issue is SPEED: RNN training measured ~5-8x SLOWER on Metal than on CPU
## (no fused kernel / high per-timestep overhead). So on Metal we DEFAULT RNN
## layers to CPU for PERFORMANCE (CPU is faster), not correctness. Override with
## options(keras3.metal_rnn_ok = TRUE) if a specific large model is faster on GPU.
## On CUDA, RNNs run on GPU (cuDNN, fast + correct). On CPU, everything is CPU.

rnn_runs_on_gpu <- function(device = detect_device()) {
  if (device == "cuda") return(TRUE)
  if (device == "metal") return(isTRUE(getOption("keras3.metal_rnn_ok", FALSE)))
  FALSE
}

## Wrap RNN-containing model *construction* (and, if needed, fit/predict) so the
## recurrent layers land on CPU when policy says so. Usage:
##   with_rnn_device(device, { model <- keras_model_sequential() %>% layer_gru(...) ... })
with_rnn_device <- function(device = detect_device(), expr) {
  expr <- substitute(expr)
  force_cpu <- (device %in% c("metal", "cuda")) && !rnn_runs_on_gpu(device)
  if (force_cpu && requireNamespace("tensorflow", quietly = TRUE)) {
    tf <- tensorflow::tf
    with(tf$device("/CPU:0"), eval(expr, envir = parent.frame()))
  } else {
    eval(expr, envir = parent.frame())
  }
}

## --- Multi-GPU strategy (CUDA only) ----------------------------------------
## Returns a tf.distribute.MirroredStrategy when device == "cuda" AND n_gpus > 1,
## else NULL. Apple exposes a single GPU, so MirroredStrategy is moot on Metal
## (1 replica) — callers should use the single-device path when this returns NULL.

multigpu_strategy <- function(device = detect_device(), n_gpus = 1L) {
  if (device != "cuda" || n_gpus <= 1) return(NULL)
  if (!requireNamespace("tensorflow", quietly = TRUE)) return(NULL)
  tryCatch(tensorflow::tf$distribute$MirroredStrategy(), error = function(e) NULL)
}

## --- XGBoost device params (modern xgboost >= 2.0 API) ----------------------
## Old code used tree_method='gpu_hist' (removed). Modern API: tree_method='hist'
## + device='cuda'|'cpu'. XGBoost has NO Metal backend, so Apple Silicon always
## gets CPU 'hist' (multi-threaded via nthread). Merge the returned list into
## your xgb.train / xgb.cv params.

xgb_device_params <- function(device = detect_device()) {
  list(
    tree_method = "hist",
    device      = if (identical(device, "cuda")) "cuda" else "cpu"
  )
}

## --- Convenience banner -----------------------------------------------------
device_report <- function() {
  d <- detect_device(refresh = TRUE)
  tf_ver <- tryCatch(as.character(tensorflow::tf_version()), error = function(e) "n/a")
  k_ver  <- tryCatch(as.character(utils::packageVersion("keras3")), error = function(e) "n/a")
  cat(sprintf(paste0(
    "deviceConfig report\n",
    "  OS/arch      : %s / %s\n",
    "  NN device    : %s\n",
    "  tensorflow   : %s\n",
    "  keras3       : %s\n",
    "  RNN on GPU   : %s  (options(keras3.metal_rnn_ok=) to override on Metal)\n",
    "  xgboost dev  : %s\n"),
    Sys.info()[["sysname"]], Sys.info()[["machine"]],
    d, tf_ver, k_ver, rnn_runs_on_gpu(d), xgb_device_params(d)$device))
  invisible(d)
}
