# Render previews using the REAL ported .pelsa_export_ggplot (post load_all), for
# both color modes, so the preview reflects the actual app export path.
#   Rscript dev/pelsa_export_prototypes/preview_real_export.R
suppressMessages(devtools::load_all(".", quiet = TRUE))

set.seed(42)
n <- 9000L
logFC <- rnorm(n, 0, 1.4)
logFC[sample(n, 60)] <- logFC[sample(n, 60)] + sample(c(-5, 5), 60, TRUE)
logP <- pmax(0, abs(rnorm(n, 0, 1)) * (1 + abs(logFC) / 3))
wing <- which(abs(logFC) > 3.2); logP[wing] <- logP[wing] + runif(length(wing), 2, 7)
adjp <- 10^(-logP) * 5
sig <- adjp < 0.05
sig_direction <- ifelse(sig & logFC > 0, "up", ifelse(sig & logFC < 0, "down", "ns"))

classes <- names(Protigy:::PELSA_FEATURE_COLORS)
df <- data.frame(
  logFC = logFC, logP = logP, adj.P.Val = adjp, Significant = sig,
  sig_direction = sig_direction,
  feature_class_primary = sample(classes, n, replace = TRUE),
  winning_accession = paste0("ACC", sample(1:400, n, replace = TRUE)),
  is_marker = FALSE, label = NA_character_, stringsAsFactors = FALSE)
mk_idx <- sample(which(logP < 2.5), 13L)
mk_hi  <- sample(which(logP > 6 & logFC < 0), 1L)
df$is_marker[c(mk_idx, mk_hi)] <- TRUE
df$label[mk_hi] <- "DHCR7_aa462"
df$label[mk_idx[1]] <- "A0A494C0F3_aa2;EBP_aa2"
df$label[mk_idx[2]] <- "VAMP8_aa25"
attr(df, "y_cutoff") <- min(df$logP[df$sig_direction != "ns"])

contrast <- "AY9944_1uM_over_AY9944_U18666A_DMSO"

g_sig <- Protigy:::.pelsa_export_ggplot(
  df, df, color_mode = "significance", label_mode = "all_markers",
  contrast = contrast, volcano_label = "All-peptide volcano")
ggplot2::ggsave("dev/pelsa_export_prototypes/real_significance.png", g_sig,
                width = 6, height = 4.5, dpi = 300)

g_feat <- Protigy:::.pelsa_export_ggplot(
  df, df, color_mode = "feature", label_mode = "all_markers",
  contrast = contrast, volcano_label = "All-peptide volcano")
ggplot2::ggsave("dev/pelsa_export_prototypes/real_feature.png", g_feat,
                width = 6, height = 4.5, dpi = 300)
cat("wrote real_significance.png + real_feature.png\n")
