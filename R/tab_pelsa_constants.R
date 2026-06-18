################################################################################
# PELSA shared constants (sourced early; referenced across section3 + woods).
################################################################################

.PELSA_GOLD          <- "#D4AF37"   # selection fill + coverage-track fill
.PELSA_GOLD_RING_W   <- 2           # same-protein peptide ring width
.PELSA_SEL_DARK_RING <- "#333333"   # the selected peptide's dark outline
.PELSA_SEL_DARK_RING_W <- 1.2
# Clicked-peptide emphasis on the volcano: SAME gold fill as its siblings, but
# a larger dot with a thicker black outline so the clicked point stands out
# (vs. the gold overlay's size 7 / 0.5px ring). Drawn as a one-point overlay
# trace on top of the gold highlight.
.PELSA_CLICK_PT_SIZE   <- 11        # clicked-point marker size (gold dot is 7)
.PELSA_CLICK_PT_RING_W <- 2         # clicked-point black-outline width (gold is 0.5)

# ---- Export tree: stage + sub-stage folder names ----------------------------
# The PELSA export writes one nested tree per ome: <ome>/pelsa/<stage>/...
# (the three section servers are merged under a single "pelsa" tab in app_server).
.PELSA_STAGE_SETUP   <- "01_setup"
.PELSA_STAGE_QC      <- "02_qc"
.PELSA_STAGE_VOLCANO <- "03_volcano"
.PELSA_SUB_VOLCANO   <- "01_volcano"
.PELSA_SUB_INTENSITY <- "02_intensity_line"
.PELSA_SUB_WOODS     <- "03_woods"
.PELSA_GRP_MARKER    <- "01_marker"
.PELSA_GRP_SIGNIF    <- "02_significant"

# Significance threshold used across the export figures (volcano / woods / the
# intensity panel split). Kept in one place so figures + footnotes stay in sync.
.PELSA_EXPORT_SIG_CUTOFF <- 0.05

# Figure-export format switches. PNG (via the ragg AGG device) is the shipping
# format; every exported figure is rasterized at this DPI. PDF output is kept in
# the code but switched OFF by default - flip .PELSA_EXPORT_PDF to TRUE to also
# emit vector PDFs alongside the PNGs (future demand).
.PELSA_EXPORT_PDF <- FALSE
.PELSA_EXPORT_DPI <- 300

# Synthetic contrast key for "significant in ANY contrast" (the contrast-
# independent intensity panel split + significant-protein union). A min-adj.P
# column named adj.P.Val.<this> is added so the existing per-contrast helpers
# (pelsa_intensity_proteins / pelsa_intensity_line_data) reuse unchanged.
.PELSA_ANY_CONTRAST <- "any_contrast"
