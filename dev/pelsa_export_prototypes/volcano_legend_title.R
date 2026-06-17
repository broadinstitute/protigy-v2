# PROTOTYPE (dev-only, NOT loaded by the package): adds a legend + title/subtitle
# to the static volcano export. Mirrors R/tab_pelsa_section3_helpers.R
# .pelsa_export_ggplot but maps color/fill INSIDE aes() so ggplot draws legends,
# and adds labs(title=, subtitle=). Run from repo root:
#   Rscript dev/pelsa_export_prototypes/volcano_legend_title.R
# Renders dev/pelsa_export_prototypes/volcano_legend_title_significance.png
#
# Once you're happy with the look, the same labs()/aes()/scale_*_manual() block
# gets ported into .pelsa_export_ggplot (ASCII-only in R/).

suppressMessages({
  library(ggplot2)
  library(ggrepel)
})

# ---- palette (verbatim from the package constants) --------------------------
SIG_UP   <- "darkred"
SIG_DOWN <- "#1f4e9c"
SIG_NS   <- "gray"
MARKER_FILL <- "#FF00FF"
MARKER_EDGE <- "black"
BG_ALPHA <- 0.8

# ---- synthetic volcano df resembling the real export ------------------------
set.seed(42)
n_bg <- 9000L
logFC <- rnorm(n_bg, 0, 1.4)
# heavier tails so a handful land in the significant wings
logFC[sample(n_bg, 60)] <- logFC[sample(n_bg, 60)] + sample(c(-5, 5), 60, TRUE)
base_p <- abs(rnorm(n_bg, 0, 1)) * (1 + abs(logFC) / 3)
logP <- pmax(0, base_p)                      # -log10(P.Value)
# push the wing points up
wing <- which(abs(logFC) > 3.2)
logP[wing] <- logP[wing] + runif(length(wing), 2, 7)

adjp <- 10^(-logP) * 5                        # crude adj.P proxy for cutoff demo
sig_cutoff <- 0.05
sig <- !is.na(adjp) & adjp < sig_cutoff
up   <- sig & logFC > 0
down <- sig & logFC < 0
sig_direction <- ifelse(up, "up", ifelse(down, "down", "ns"))

df <- data.frame(
  logFC = logFC, logP = logP, adj.P.Val = adjp,
  sig_direction = sig_direction,
  is_marker = FALSE,
  label = NA_character_,
  stringsAsFactors = FALSE
)

# ~15 markers scattered (mostly low-significance, like the screenshot)
mk_idx <- sample(which(logP < 2.5), 13L)
mk_hi  <- sample(which(logP > 6 & logFC < 0), 1L)   # one high marker like DHCR7
df$is_marker[c(mk_idx, mk_hi)] <- TRUE
df$label[mk_hi] <- "DHCR7_aa462"
df$label[mk_idx[1]] <- "A0A494C0F3_aa2;EBP_aa2"
df$label[mk_idx[2]] <- "VAMP8_aa25"

# dashed cutoff line (-log10 of the worst passing p) -- mimic attr(full,"y_cutoff")
y_cut <- min(df$logP[df$sig_direction != "ns"])

# ============================================================================ #
#  THE PROTOTYPE BUILD  (this is the block that ports into .pelsa_export_ggplot)
# ============================================================================ #

# Human-readable significance labels + the manual scale (fixed display order).
SIG_LABELS <- c(down = "Downregulated",
                ns   = "Non-significant",
                up   = "Upregulated")
SIG_VALUES <- c("Downregulated"   = SIG_DOWN,
                "Non-significant" = SIG_NS,
                "Upregulated"     = SIG_UP)

bg <- df[!df$is_marker, , drop = FALSE]
mk <- df[ df$is_marker, , drop = FALSE]
bg$sig_label <- factor(SIG_LABELS[bg$sig_direction], levels = names(SIG_VALUES))

contrast     <- "AY9944_1uM_over_AY9944_U18666A_DMSO"
title_txt    <- gsub("_over_", " vs ", contrast, fixed = TRUE)
subtitle_txt <- "All-peptide volcano | significance coloring"

gg <- ggplot()

# background points -- color mapped INSIDE aes so it gets a legend
gg <- gg + geom_point(
  data = bg,
  aes(x = logFC, y = logP, color = sig_label),
  alpha = BG_ALPHA, size = 1)

# dashed significance cutoff + its annotation (right end, just below the line)
if (is.finite(y_cut)) {
  gg <- gg + geom_hline(yintercept = y_cut, linetype = "dashed", color = "grey40")
  gg <- gg + annotate(
    "text", x = Inf, y = y_cut,
    label = "adj.P < 0.05", hjust = 1.15, vjust = 1.5,
    size = 2, fontface = "bold", color = "grey30")
}

# markers -- shape 21, fill mapped INSIDE aes (constant) so it gets its own legend
if (nrow(mk) > 0L) {
  gg <- gg + geom_point(
    data = mk,
    aes(x = logFC, y = logP, fill = "Marker"),
    shape = 21, size = 2.4, stroke = 0.5, color = MARKER_EDGE)
}

# marker labels -- white box, black outline + text, black segment; force=20 to
# spread crowded labels apart (see ggrepel docs).
lab <- mk[!is.na(mk$label) & nzchar(mk$label), , drop = FALSE]
if (nrow(lab) > 0L) {
  gg <- gg + geom_label_repel(
    data = lab, aes(x = logFC, y = logP, label = label),
    size = 2.6, force = 20, max.overlaps = Inf,
    fill = "white", color = "black",
    label.size = 0.3, label.padding = 0.18,
    min.segment.length = 0, segment.size = 0.3, segment.color = "black")
}

gg <- gg +
  scale_color_manual(name = NULL, values = SIG_VALUES, drop = FALSE) +
  scale_fill_manual(name = NULL, values = c("Marker" = MARKER_FILL)) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    fill  = guide_legend(order = 2,
                         override.aes = list(shape = 21, size = 3, color = MARKER_EDGE))) +
  labs(x = "logFC", y = "-log10(P.Value)",
       title = title_txt, subtitle = subtitle_txt) +
  theme_bw() +
  theme(
    plot.title.position = "plot",   # center over the FULL figure, not the panel
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
    axis.title = element_text(size = 9, face = "bold"),
    axis.text  = element_text(size = 6),
    legend.position = "right",
    legend.title  = element_blank(),
    legend.text   = element_text(size = 6),
    legend.key = element_blank(),
    # compact legend: small keys, tight spacing between entries/blocks
    legend.key.size   = unit(8, "pt"),
    legend.key.spacing.y = unit(1, "pt"),
    legend.spacing.y  = unit(2, "pt"),
    legend.margin     = margin(2, 4, 2, 4),
    legend.box.spacing = unit(4, "pt"),
    # black outline around the whole legend panel
    legend.box.background = element_rect(color = "black", fill = NA, linewidth = 0.4),
    legend.box.margin = margin(2, 2, 2, 2))

out <- "dev/pelsa_export_prototypes/volcano_legend_title_significance.png"
ggsave(out, gg, width = 6, height = 4.5, dpi = 300)
cat("wrote", out, "\n")
