# =============================================================================
# Standalone benchmark: compare UniProt fetch STRATEGIES on a fixed sample.
# Measures wall-clock + correctness (row-for-row) vs a per-sample reference.
# Exploration only -- no app code touched. Run: Rscript dev/fetch_strategy_benchmark.R
# =============================================================================
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

BASE <- "https://rest.uniprot.org"
UA   <- "pelsa_bench/0.1"
FASTA <- file.path("inst/database/10090/fasta",
                   "UniProt.mouse.20171228.RISnrNF.553smORFs.264contams.fasta")
SAMPLE_N <- 2000L   # fixed representative sample

`%||%` <- function(a, b) if (is.null(a)) b else a
ACC_RE <- "^([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$"
is_valid <- function(x) !is.na(x) & grepl(ACC_RE, x)
iso_base <- function(x) sub("-[0-9]+$", "", x)

# --- shared parser (same logic across all strategies; we compare TRANSPORT) --
SCORES <- c(active_or_binding_site=5L,catalytic_domain=3L,folded_domain=2L,
  region_or_motif=1L,repeat_or_coiled_coil=-1L,transmembrane_or_signal=0L,
  low_complexity_or_disorder=-3L,other=0L)
fclass <- function(ft, d) {
  ft<-tolower(trimws(ft%||%"")); d<-tolower(trimws(d%||%""))
  dis<-grepl("low complexity|compositionally biased|disordered",d)
  site<-c("active site","binding site","metal binding","nucleotide binding","site","dna binding")
  tm<-c("transmembrane","signal peptide","topological domain","intramembrane","signal")
  rp<-c("repeat","coiled-coil","coiled coil"); ct<-c("kinase","methyltransferase","transferase","atpase","helicase","protease","dehydrogenase")
  if(ft=="compositional bias")return("low_complexity_or_disorder")
  if(ft%in%site)return("active_or_binding_site"); if(ft%in%tm)return("transmembrane_or_signal")
  if(dis)return("low_complexity_or_disorder"); if(ft%in%rp)return("repeat_or_coiled_coil")
  if(ft=="domain")return(if(any(vapply(ct,function(k)grepl(k,d,fixed=TRUE),logical(1))))"catalytic_domain" else "folded_domain")
  if(ft%in%c("region","motif"))return("region_or_motif"); "other"
}
parse_entry <- function(e) {
  acc<-e$primaryAccession%||%""; fs<-e$features; if(is.null(fs)||!length(fs))return(NULL)
  rows<-lapply(fs,function(f){ sv<-f$location$start$value; ev<-f$location$end$value
    if(is.null(sv)||is.null(ev))return(NULL); ft<-f$type%||%""; d<-f$description%||%""
    if(!nzchar(d)&&!is.null(f$ligand$name))d<-f$ligand$name
    sm<-f$location$start$modifier%||%"EXACT"; em<-f$location$end$modifier%||%"EXACT"
    fc<-fclass(ft,d)
    data.frame(accession=acc,feature_type=ft,start=as.integer(sv),end=as.integer(ev),
      description=d,feature_class=fc,class_score=as.integer(SCORES[[fc]]),
      coord_quality=if(sm=="EXACT"&&em=="EXACT")"exact" else "fuzzy",stringsAsFactors=FALSE)})
  do.call(rbind,Filter(Negate(is.null),rows))
}
collate <- function(entries) {
  fl<-Filter(Negate(is.null),lapply(entries,parse_entry))
  out<-if(length(fl))do.call(rbind,fl) else data.frame()
  if(nrow(out))out<-out[order(out$accession,out$feature_type,out$start,out$end,out$description),]
  rownames(out)<-NULL; out
}

# --- read sample (deterministic slice of the valid base universe) ------------
read_keys <- function(p){ l<-readLines(p,warn=FALSE); h<-sub("^>","",l[startsWith(l,">")])
  ft<-sub("\\s.*$","",h); hp<-grepl("\\|",ft)
  k<-ifelse(hp,sub("^[^|]*\\|([^|]*)\\|.*$","\\1",ft),ft); unique(k[!is.na(k)&nzchar(k)]) }
all_keys <- read_keys(FASTA)
query_universe <- sort(unique(iso_base(all_keys[is_valid(all_keys)])))
set.seed(42)
sample_accs <- sort(query_universe[round(seq(1, length(query_universe), length.out = SAMPLE_N))])
cat(sprintf("Universe %d valid-base; sample %d\n", length(query_universe), length(sample_accs)))

timeit <- function(label, fn) {
  t0 <- Sys.time(); res <- tryCatch(fn(), error=function(e) {cat("ERR:",conditionMessage(e),"\n"); NULL})
  el <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  if (is.null(res)) return(list(label=label, secs=el, rows=NA, accs=NA, df=NULL))
  list(label=label, secs=round(el,1), rows=nrow(res), accs=length(unique(res$accession)), df=res)
}

mk_req <- function(path) request(BASE) |> req_url_path(path) |> req_user_agent(UA) |>
  req_throttle(capacity=10, fill_time_s=1) |>
  req_retry(max_tries=5, is_transient=function(r) resp_status(r) %in% c(429,500,502,503,504)) |>
  req_error(is_error=function(r) resp_status(r) >= 500)

# ---- STRATEGY A: /search batch=100, serial + cursor (mirrors app path) -------
strat_search_serial <- function(accs, bs=100L) {
  batches <- split(accs, ceiling(seq_along(accs)/bs)); entries<-list()
  for (b in batches) {
    req <- mk_req("/uniprotkb/search") |>
      req_url_query(query=paste0("accession:(",paste(b,collapse=" OR "),")"), format="json", size=bs)
    repeat {
      rp <- req_perform(req); if (resp_status(rp)>=400) break
      pg <- resp_body_json(rp); entries <- c(entries, pg$results %||% list())
      lk <- resp_header(rp,"Link"); if (is.null(lk)||!grepl("next",lk)) break
      req <- request(sub('.*<([^>]+)>;\\s*rel="next".*',"\\1",lk)) |> req_user_agent(UA) |>
        req_throttle(capacity=10,fill_time_s=1) |> req_error(is_error=function(r) FALSE)
    }
  }
  collate(entries)
}

# ---- STRATEGY B: /stream batch=100, serial (no cursor; one resp per batch) ---
strat_stream_serial <- function(accs, bs=100L) {
  batches <- split(accs, ceiling(seq_along(accs)/bs)); entries<-list()
  for (b in batches) {
    rp <- mk_req("/uniprotkb/stream") |>
      req_url_query(query=paste0("accession:(",paste(b,collapse=" OR "),")"), format="json") |>
      req_perform()
    if (resp_status(rp)<400) entries <- c(entries, resp_body_json(rp)$results %||% list())
  }
  collate(entries)
}

# ---- STRATEGY C: /search batch=100 PARALLEL (req_perform_parallel) -----------
strat_search_parallel <- function(accs, bs=100L, pool=5L) {
  batches <- split(accs, ceiling(seq_along(accs)/bs))
  reqs <- lapply(batches, function(b) mk_req("/uniprotkb/stream") |>
    req_url_query(query=paste0("accession:(",paste(b,collapse=" OR "),")"), format="json"))
  resps <- req_perform_parallel(reqs, max_active=pool, on_error="continue")
  entries <- list()
  for (rp in resps) if (inherits(rp,"httr2_response") && resp_status(rp)<400)
    entries <- c(entries, resp_body_json(rp)$results %||% list())
  collate(entries)
}

# ---- STRATEGY D: ID-mapping bulk submit (all accs in ONE job) ----------------
strat_idmapping <- function(accs) {
  run <- request(BASE) |> req_url_path("/idmapping/run") |> req_user_agent(UA) |>
    req_body_multipart(ids=paste(accs,collapse=","), from="UniProtKB_AC-ID", to="UniProtKB") |>
    req_perform()
  job <- resp_body_json(run)$jobId
  repeat { st <- request(BASE)|>req_url_path(paste0("/idmapping/status/",job))|>req_user_agent(UA)|>
      req_error(is_error=function(r)FALSE)|>req_perform()
    sj <- resp_body_json(st)
    if (!is.null(sj$jobStatus) && sj$jobStatus=="RUNNING") { Sys.sleep(1); next }
    break }
  # paginate results (size up to 500)
  url <- paste0(BASE,"/idmapping/uniprotkb/results/",job,"?format=json&size=500")
  entries <- list()
  repeat {
    rp <- request(url)|>req_user_agent(UA)|>req_throttle(capacity=10,fill_time_s=1)|>
      req_error(is_error=function(r) resp_status(r)>=500)|>req_perform()
    if (resp_status(rp)>=400) break
    pg <- resp_body_json(rp)
    tos <- lapply(pg$results, function(r) r$to)   # unwrap {from,to}
    entries <- c(entries, tos)
    lk <- resp_header(rp,"Link"); if (is.null(lk)||!grepl("next",lk)) break
    url <- sub('.*<([^>]+)>;\\s*rel="next".*',"\\1",lk)
  }
  collate(entries)
}

# ---- run reference (Strategy A IS the established gold-standard transport) ----
cat("\n--- running strategies on", SAMPLE_N, "accessions ---\n")
A <- timeit("A /search serial+cursor (app path)", function() strat_search_serial(sample_accs))
B <- timeit("B /stream serial (no cursor)",        function() strat_stream_serial(sample_accs))
C <- timeit("C /stream parallel pool=5",           function() strat_search_parallel(sample_accs, pool=5L))
D <- timeit("D idmapping bulk (one job)",          function() strat_idmapping(sample_accs))

ref <- A$df  # A is the proven-correct transport (== gold standard logic)
cmp <- function(x) {
  if (is.null(x$df)) return("FAILED")
  ka <- paste(ref$accession,ref$feature_type,ref$start,ref$end,ref$description)
  kx <- paste(x$df$accession,x$df$feature_type,x$df$start,x$df$end,x$df$description)
  sprintf("only_ref=%d only_x=%d identical=%s",
    length(setdiff(ka,kx)), length(setdiff(kx,ka)),
    identical(as.data.frame(ref), as.data.frame(x$df)))
}
cat("\n================ STRATEGY BENCHMARK ================\n")
for (r in list(A,B,C,D)) cat(sprintf("%-34s %6ss  rows=%-7s accs=%-6s  %s\n",
  r$label, r$secs, r$rows%||%"NA", r$accs%||%"NA", if(identical(r$label,A$label)) "(reference)" else cmp(r)))
cat("===================================================\n")
