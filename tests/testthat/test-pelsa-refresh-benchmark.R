################################################################################
# Benchmark / runtime characterization for the PELSA UniProt-annotation refresh
# (the Setup tab "Maintenance: UniProt annotation library" control), focused on
# the HUMAN species.
#
# WHY THIS FILE EXISTS
#   The refresh is the single slowest user-facing PELSA action. Its cost is
#   dominated by the network fetch in pelsa_fetch_uniprot(), which today issues
#   ONE HTTP request PER ACCESSION, serially, throttled to ~10 req/s. For the
#   human FALLBACK universe (the whole FASTA proteome, ~70k accessions) that is
#   ~70k / 10 = ~7000 s ~= 2 h floor before any per-request latency or retries.
#
#   These tests measure what we CAN measure deterministically and offline:
#     1. the orchestration overhead the refresh adds ON TOP of the network
#        (merge + write + progress), to prove the pipeline itself is cheap and
#        the network is the whole story; and
#     2. a calibrated MODEL of wall-clock under the current serial design vs a
#        batched-stream design, so the speed-up is quantified, not hand-waved.
#
#   NO LIVE NETWORK: every fetch goes through the injected `fetch_fn` seam. The
#   "network" is a calibrated sleep/arithmetic model, never a real request, so
#   this file is hermetic and CI-safe (it is also tagged slow-ish but bounded:
#   the real-time sleeps are tiny and only run on a small sample).
#
# These are characterization tests, not pass/fail correctness gates: they assert
# only ROBUST inequalities (batched < serial; overhead << network) that hold by
# construction, so they are not flaky on a busy machine.
################################################################################

library(testthat)

# ---- calibration constants (documented model, not magic numbers) -------------
# Per-request wall-clock the live design pays per accession: throttle slot
# (1/rate s) PLUS a round-trip latency. We model both; only their RATIO matters
# for the inequality assertions, so absolute values need only be plausible.
.BENCH_RATE_PER_S        <- 10      # req_throttle(capacity = 10, fill_time_s = 1)
.BENCH_RTT_S             <- 0.12    # typical UniProt JSON RTT under throttle
.BENCH_STREAM_PAGE       <- 500L    # accessions per batched /stream page (model)
.BENCH_STREAM_PAGE_RTT_S <- 0.9     # one big page's RTT (bigger payload)

# Wall-clock model for the CURRENT serial, one-accession-per-request fetcher.
.bench_serial_seconds <- function(n) {
  n * (1 / .BENCH_RATE_PER_S + .BENCH_RTT_S)
}

# Wall-clock model for a BATCHED /stream fetcher (ceil(n / page) page requests,
# each paying one big-page RTT; throttle is a non-binding ~1 req/s for pages).
.bench_batched_seconds <- function(n, page = .BENCH_STREAM_PAGE) {
  n_pages <- ceiling(n / page)
  n_pages * .BENCH_STREAM_PAGE_RTT_S
}

# A canned 8-col feature frame for K accessions (orchestration input; the shape
# the parser/fetcher returns). Cheap to build for large K.
.bench_feature_df <- function(accs) {
  k <- length(accs)
  data.frame(
    accession     = accs,
    feature_type  = rep("domain", k),
    start         = rep(10L, k),
    end           = rep(120L, k),
    description   = rep("kinase domain", k),
    feature_class = rep("catalytic_domain", k),
    class_score   = rep(3L, k),
    coord_quality = rep("exact", k),
    stringsAsFactors = FALSE
  )
}

# ---- 1. Orchestration overhead is negligible vs the network ------------------

test_that("refresh orchestration (merge + write) is fast; the network dominates", {
  skip_on_cran()
  # A realistic single-experiment universe size (proteins in a typical dataset).
  n <- 5000L
  accs <- sprintf("P%05d", seq_len(n))
  species_dir <- tempfile("pelsa_bench_human_")
  dir.create(species_dir, recursive = TRUE)

  # Injected "fetch" that does NO real I/O: returns the canned frame instantly.
  # This isolates the orchestration cost (universe merge + write_tsv + schema)
  # from the network so we can show the pipeline itself is not the bottleneck.
  fake_fetch <- function(accessions) {
    list(features = .bench_feature_df(accessions), unresolved = character(0))
  }

  t <- system.time(
    res <- pelsa_refresh_species_cache(
      species = "human", universe = accs, species_dir = species_dir,
      fetch_fn = fake_fetch, existing = NULL, progress = NULL
    )
  )
  orchestration_s <- unname(t["elapsed"])

  expect_equal(res$n_features, n)
  expect_true(file.exists(res$path))

  # The network the live fetcher WOULD pay for the same universe.
  network_serial_s <- .bench_serial_seconds(n)

  # Orchestration must be a tiny fraction of the network it sits on top of. We
  # assert a very loose bound (<10% of modelled network, and < 5 s absolute) so
  # this is robust on slow CI, while still proving the network is the story.
  expect_lt(orchestration_s, 0.10 * network_serial_s)
  expect_lt(orchestration_s, 5)

  message(sprintf(
    "[bench] human n=%d: orchestration=%.3fs  vs modelled serial network=%.0fs (%.1f min)",
    n, orchestration_s, network_serial_s, network_serial_s / 60))
})

# ---- 2. Serial per-accession design: human runtime is hours, not minutes -----

test_that("human FALLBACK universe (~70k accessions) blows past 'several minutes'", {
  # The no-datasets-uploaded fallback fetches the whole FASTA proteome. The
  # committed human FASTA has ~70k headers; model the serial wall-clock.
  n_human_fallback <- 70000L
  serial_s <- .bench_serial_seconds(n_human_fallback)

  # Floor is well over an hour - far beyond the "several minutes per species"
  # the UI promises. This test DOCUMENTS the gap (and guards against anyone
  # quietly assuming it's fast).
  expect_gt(serial_s, 60 * 60)   # > 1 hour
  message(sprintf(
    "[bench] human fallback n=%d: serial model=%.0fs (%.1f h)",
    n_human_fallback, serial_s, serial_s / 3600))
})

test_that("a realistic single-dataset universe already exceeds 'several minutes'", {
  # Even the dataset-driven (non-fallback) path is slow: a mid-size experiment
  # annotates several thousand proteins.
  n_dataset <- 8000L
  serial_s <- .bench_serial_seconds(n_dataset)
  expect_gt(serial_s, 5 * 60)    # > 5 minutes for a single mid-size dataset
  message(sprintf(
    "[bench] dataset n=%d: serial model=%.0fs (%.1f min)",
    n_dataset, serial_s, serial_s / 60))
})

# ---- 3. Batched /stream design: order-of-magnitude faster, same coverage -----

test_that("batched /stream model is >=20x faster than serial for human", {
  for (n in c(5000L, 8000L, 70000L)) {
    serial_s  <- .bench_serial_seconds(n)
    batched_s <- .bench_batched_seconds(n)
    speedup   <- serial_s / batched_s
    expect_gt(speedup, 20)
    message(sprintf(
      "[bench] n=%6d: serial=%8.0fs  batched=%7.1fs  speedup=%.0fx",
      n, serial_s, batched_s, speedup))
  }
})

# ---- 4. Batching does NOT change the parsed result (accuracy preserved) ------

test_that("parsing N entries in one batch == parsing them per-accession", {
  # The accuracy guarantee behind batching: pelsa_parse_uniprot_json_batch over
  # a multi-entry /stream 'results' array yields the SAME 8-col rows as parsing
  # each entry alone and rbinding. (The classifier is per-feature and pure, so a
  # batched fetch cannot change a single class/score/coord.)
  entry <- function(acc) list(
    primaryAccession = acc,
    features = list(list(
      type = "Domain", description = "Protein kinase domain",
      location = list(start = list(value = 10L, modifier = "EXACT"),
                      end   = list(value = 120L, modifier = "EXACT"))
    ))
  )
  entries <- lapply(c("P00001", "P00002", "P00003"), entry)

  per_accession <- do.call(rbind, lapply(entries, pelsa_parse_uniprot_json))
  rownames(per_accession) <- NULL
  batched <- pelsa_parse_uniprot_json_batch(entries)

  expect_identical(batched, per_accession)
  expect_identical(sort(unique(batched$accession)),
                   c("P00001", "P00002", "P00003"))
  expect_true(all(batched$feature_class == "catalytic_domain"))
})
