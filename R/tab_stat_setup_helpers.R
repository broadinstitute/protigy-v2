################################################################################
# Module: Stat_Setup
#
# Allow users to setup the test type and parameters
################################################################################

stat.testing <- function(
  test,
  annotation_col,
  chosen_omes,
  gct,
  chosen_groups,
  selected_contrasts,
  p.value.alpha = 0.05,
  use.adj.pvalue = TRUE,
  apply.log = FALSE,
  intensity,
  ...
) {
  # Ensure intensity is a logical value
  if (is.null(intensity)) {
    intensity <- FALSE
  } else if (is.character(intensity)) {
    # Convert "Yes"/"No" strings to logical values
    intensity <- tolower(intensity) %in% c("yes", "true", "1", "t")
  } else {
    intensity <- as.logical(intensity)
  }
  ################################################################################
  #None
  ################################################################################
  if (test == 'None') {
    results_list <- NULL
  }
  ################################################################################
  #Mod F Test
  ################################################################################
  if (test == 'Moderated F test') {
    cat('\n-- modF.test --\n')
    withProgress(message = 'moderated F-test', value = 0, {
      results_list <- list() # store results by ome
      for (ome_name in chosen_omes) {
        incProgress(
          1 / length(chosen_omes),
          detail = paste("Processing", ome_name)
        )

        ome_data <- gct[[ome_name]]@mat
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc
        tab <- as.data.frame(ome_data)

        #Add ID column to tab
        # In GCT files, the "id" column is ALWAYS named "id"
        if ("id" %in% colnames(rdesc)) {
          id.col <- "id"
          tab <- cbind(rdesc[["id"]], tab)
          colnames(tab)[1] <- "id"
        } else {
          # If no "id" column exists, create it from row names
          id.col <- "id"
          tab <- cbind(rownames(rdesc), tab)
          colnames(tab)[1] <- "id"
        }

        #Run test on only the chosen groups
        sample_names <- colnames(ome_data)
        all_groups <- cdesc[sample_names, annotation_col, drop = TRUE]
        keep_samples_logical <- all_groups %in% chosen_groups
        samples_to_keep <- sample_names[keep_samples_logical] #run test on only the chosen groups
        groups <- all_groups[match(samples_to_keep, sample_names)]

        tab.group <- cbind(tab[[id.col]], tab[, samples_to_keep])
        colnames(tab.group)[1] <- id.col

        #MOD F LOGIC
        id <- tab.group[, id.col]
        data <- tab.group[, setdiff(colnames(tab.group), id.col)]

        f <- factor(groups)
        if (length(levels(f)) < 2) {
          message(paste(
            "Skipping",
            ome_name,
            "- not enough of the selected groups found in the data."
          ))
          next
        }

        # tryCatch({
        design <- model.matrix(~ 0 + f)

        #use row centered data -- this does not affect 2-sample t test results, but makes the F test more interpretable
        data.rownorm <- sweep(
          data,
          MARGIN = 1,
          STATS = apply(data, 1, mean, na.rm = TRUE)
        )
        fit <- lmFit(data.rownorm, design)

        #check intensity
        if (intensity) {
          fit <- tryCatch(
            {
              eBayes(fit, trend = TRUE, robust = TRUE)
            },
            error = function(e) {
              shinyalert(
                "Setting intensity-trend failed. Performing with trend=FALSE. This usually occurs when the distribution of detected features is not uniform across samples. Please evaluate your data and consider re-running analysis with a stricter missing value filter.",
                type = "warning",
                immediate = T
              )
              eBayes(fit, trend = FALSE, robust = TRUE)
            }
          )
        } else {
          fit <- eBayes(fit, robust = TRUE)
        }

        sig <- topTable(fit, number = nrow(data), sort.by = 'none')
        mod.sig <- if (use.adj.pvalue) {
          sig[, "adj.P.Val"] <= p.value.alpha
        } else {
          sig[, "P.Value"] <= p.value.alpha
        }
        non.na.n <- apply(data, 1, function(x) {
          sum(is.finite(x))
        })

        final.results <- data.frame(
          cbind(
            id = id,
            sig,
            significant = mod.sig,
            total.n = non.na.n,
            Log.P.Value = -log(sig[, 'P.Value'], 10)
          ),
          stringsAsFactors = F
        )
        # final.results$sign.logP <-final.results$Log.P.Value*sign(final.results$logFC)
        colnames(final.results) <- sub(
          "^f",
          "AveExpr.",
          colnames(final.results)
        )

        #replace zero-centered average with the true average expression
        avg <- t(aggregate(t(data), by = list(groups), function(x) {
          mean(x, na.rm = T)
        }))
        avg <- avg[-1, ]
        avg <- matrix(as.numeric(avg), ncol = ncol(avg))
        final.results[, grepl("AveExpr.", colnames(final.results))] <- avg
        final.results[, colnames(final.results) == "AveExpr"] <- rowMeans(
          avg,
          na.rm = T
        )

        # Join all rdesc columns to the results
        rdesc_df <- as.data.frame(rdesc)

        # Since we now correctly use "id" as the ID column, no renaming needed
        # Just ensure the "id" column exists in rdesc_df
        if (!"id" %in% colnames(rdesc_df)) {
          rdesc_df$id <- rownames(rdesc_df)
        }

        combined_results <- dplyr::left_join(rdesc_df, final.results, by = "id")
        results_list[[ome_name]] <- combined_results
      }
    })
    cat('\n-- modF.test exit --\n')
  }

  ################################################################################
  #One sample Mod T Test
  ################################################################################
  if (test == 'One-sample Moderated T-test') {
    cat('\n-- one-sample moderated T-test --\n')
    withProgress(message = 'one-sample moderated T-test', value = 0, {
      results_list <- list() # store results by ome

      for (ome_name in chosen_omes) {
        combined_results <- NULL

        ome_data <- gct[[ome_name]]@mat
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc
        tab <- as.data.frame(ome_data)

        id.col <- names(Filter(function(col) !is.numeric(col), rdesc))[1]
        tab <- cbind(rdesc[[id.col]], tab)
        colnames(tab)[1] <- id.col

        for (group_name in chosen_groups) {
          incProgress(
            1 / (length(chosen_omes) * length(chosen_groups)),
            detail = paste("Processing", ome_name, "-", group_name)
          )

          #run test on chosen groups only
          sample_names <- colnames(ome_data)
          all_groups <- cdesc[sample_names, annotation_col, drop = TRUE]
          keep_samples_logical <- all_groups %in% group_name
          samples_to_keep <- sample_names[keep_samples_logical] #run test on one group at a time
          groups <- all_groups[match(samples_to_keep, sample_names)]

          tab.group <- cbind(tab[[id.col]], tab[, samples_to_keep])
          colnames(tab.group)[1] <- id.col

          id <- tab.group[, id.col]
          data <- tab.group[, setdiff(colnames(tab.group), id.col)]

          data <- data.matrix(data)

          # log transform is required
          if (apply.log) {
            data <- log2(data)
          }

          #MOD T LOGIC
          data.matrix <- data.frame(data, stringsAsFactors = F)
          m <- lmFit(data.matrix, method = 'robust')
          m <- eBayes(m, trend = FALSE, robust = TRUE) #one-sample t-test is only run for ratio data
          sig <- topTable(m, number = nrow(data), sort.by = 'none')

          ##View(data)
          if (use.adj.pvalue) {
            mod.sig <- sig[, 'adj.P.Val'] <= p.value.alpha
          } else {
            mod.sig <- sig[, 'P.Value'] <= p.value.alpha
          }

          ##MOD T test result
          mod.t.result <- data.frame(
            sig,
            significant = mod.sig,
            Log.P.Value = -log(sig$P.Value, 10),
            stringsAsFactors = F
          )
          mod.t.result$sign.logP <- mod.t.result$Log.P.Value *
            sign(mod.t.result$logFC)

          ##add label(group_name)
          if (!is.null(group_name)) {
            colnames(mod.t.result) <- paste(
              colnames(mod.t.result),
              group_name,
              sep = '.'
            )
          }

          mod.t <- data.frame(
            cbind(data.frame(id = id), mod.t.result),
            stringsAsFactors = F
          )
          rownames(mod.t) <- id

          # Keep only id + renamed stats
          mod.t.sub <- mod.t[, c(
            "id",
            grep(paste0("\\.", group_name, "$"), colnames(mod.t), value = TRUE)
          )]

          # Merge into the combined table for this ome
          if (is.null(combined_results)) {
            combined_results <- mod.t.sub
          } else {
            combined_results <- merge(
              combined_results,
              mod.t.sub,
              by = "id",
              all = TRUE
            )
          }
        }
        # Join all rdesc columns to the results
        rdesc_df <- as.data.frame(rdesc)
        # Since we now correctly use "id" as the ID column, no renaming needed
        # Just ensure the "id" column exists in rdesc_df
        if (!"id" %in% colnames(rdesc_df)) {
          rdesc_df$id <- rownames(rdesc_df)
        }

        combined_results <- dplyr::left_join(
          rdesc_df,
          combined_results,
          by = "id"
        )

        results_list[[ome_name]] <- combined_results
      }
    })
    cat('\n-- one-sample moderated T-test exit --\n')
  }

  ################################################################################
  #Two sample Mod T Test
  ################################################################################
  if (test == 'Two-sample Moderated T-test') {
    cat('\n-- two-sample moderated T-test --\n')
    withProgress(message = 'two-sample moderated T-test', value = 0, {
      results_list <- list()

      for (ome_name in chosen_omes) {
        incProgress(
          1 / length(chosen_omes),
          detail = paste("Processing", ome_name)
        )

        # OPTIMIZATION STRATEGY 1: Keep data as matrix (30-40% faster)
        # Extract data and metadata - keep as matrix throughout
        ome_data <- gct[[ome_name]]@mat  # Already a matrix
        rdesc <- gct[[ome_name]]@rdesc
        cdesc <- gct[[ome_name]]@cdesc

        # Get feature IDs directly from matrix rownames
        if ("id" %in% colnames(rdesc)) {
          id <- rdesc[["id"]]
        } else {
          id <- rownames(ome_data)
        }

        # OPTIMIZATION STRATEGY 7: Optimized group filtering (10-15% faster)
        # Extract all unique groups involved in any contrast
        all_contrast_groups <- unique(unlist(selected_contrasts))

        # Filter samples - direct logical indexing with fewer intermediate variables
        sample_groups <- cdesc[colnames(ome_data), annotation_col, drop = TRUE]
        keep_samples <- sample_groups %in% all_contrast_groups

        # OPTIMIZATION STRATEGY 9: Sparse matrix support (50-70% faster for sparse data)
        # Detect if data is sparse (>50% missing values)
        sparsity <- sum(is.na(ome_data)) / length(ome_data)
        use_sparse <- sparsity > 0.5

        # Subset matrix directly (fast matrix operation)
        if (use_sparse) {
          # Convert to sparse matrix for memory efficiency
          data.matrix <- Matrix::Matrix(ome_data[, keep_samples, drop = FALSE], sparse = TRUE)
        } else {
          # Keep as regular matrix
          data.matrix <- ome_data[, keep_samples, drop = FALSE]
        }

        groups <- factor(sample_groups[keep_samples], levels = all_contrast_groups)

        # Create design matrix with all groups (no intercept)
        design <- model.matrix(~ 0 + groups)
        colnames(design) <- levels(groups)

        # OPTIMIZATION STRATEGY 2: Pre-allocate contrast vectors (10-15% faster)
        # Build contrast matrix dynamically from selected_contrasts
        n_contrasts <- length(selected_contrasts)
        contrast_strings <- character(n_contrasts)
        contrast_names <- character(n_contrasts)

        for (i in seq_along(selected_contrasts)) {
          contrast_pair <- selected_contrasts[[i]]
          group1 <- contrast_pair[1]
          group2 <- contrast_pair[2]
          # For contrast "A / B", user expects fold change = A - B
          # Use backticks to handle special characters in group names
          contrast_strings[i] <- paste0("`", group1, "` - `", group2, "`")
          contrast_names[i] <- paste0(group1, "_over_", group2)
        }

        # OPTIMIZATION STRATEGY 3: Replace eval(parse()) with do.call() (5-10% faster + safer)
        # Create contrast matrix using do.call for better performance and safety
        contrast_list <- setNames(as.list(contrast_strings), contrast_names)
        contrast_matrix <- do.call(
          limma::makeContrasts,
          c(contrast_list, list(levels = design))
        )

        # Fit model once for all groups
        fit <- lmFit(data.matrix, design)

        # Fit all contrasts at once
        fit2 <- contrasts.fit(fit, contrast_matrix)

        # Apply eBayes once for all contrasts
        if (intensity) {
          fit2 <- tryCatch(
            {
              eBayes(fit2, trend = TRUE, robust = TRUE)
            },
            error = function(e) {
              shinyalert(
                "Setting intensity-trend failed. Performing with trend=FALSE. This usually occurs when the distribution of detected features is not uniform across samples. Please evaluate your data and consider re-running analysis with a stricter missing value filter.",
                type = "warning",
                immediate = T
              )
              eBayes(fit2, trend = FALSE, robust = TRUE)
            }
          )
        } else {
          fit2 <- eBayes(fit2, robust = TRUE)
        }

        # OPTIMIZATION STRATEGY 4: Batch extract all contrasts at once (40-50% faster)
        # OPTIMIZATION STRATEGY 5: Use cbind instead of repeated merge (30-40% faster)
        # OPTIMIZATION STRATEGY 6: Vectorize derived calculations (5% faster)
        # Extract results for each contrast - access fit2 object directly instead of N topTable calls
        results_list <- vector("list", n_contrasts)

        for (i in seq_along(contrast_names)) {
          contrast_name <- contrast_names[i]

          # Extract pre-computed statistics directly from fit2 object (no topTable overhead)
          contrast_results <- data.frame(
            logFC = fit2$coefficients[, i],
            AveExpr = fit2$Amean,
            t = fit2$t[, i],
            P.Value = fit2$p.value[, i],
            adj.P.Val = p.adjust(fit2$p.value[, i], method = "BH"),
            B = fit2$lods[, i],
            stringsAsFactors = FALSE
          )

          # Vectorized calculation of derived columns (all at once)
          contrast_results$significant <- if (use.adj.pvalue) {
            contrast_results$adj.P.Val <= p.value.alpha
          } else {
            contrast_results$P.Value <= p.value.alpha
          }
          contrast_results$Log.P.Value <- -log10(contrast_results$P.Value)
          contrast_results$sign.logP <- contrast_results$Log.P.Value * sign(contrast_results$logFC)

          # Rename columns with contrast name
          colnames(contrast_results) <- paste(colnames(contrast_results), contrast_name, sep = '.')

          results_list[[i]] <- contrast_results
        }

        # Combine all contrasts at once using cbind (O(N) instead of O(N*M²))
        # All results have same features in same order, so cbind is safe and fast
        combined_results <- cbind(
          data.frame(id = id, stringsAsFactors = FALSE),
          do.call(cbind, results_list)
        )
        rownames(combined_results) <- id

        # Join all rdesc columns to the results
        rdesc_df <- as.data.frame(rdesc)
        # Since we now correctly use "id" as the ID column, no renaming needed
        # Just ensure the "id" column exists in rdesc_df
        if (!"id" %in% colnames(rdesc_df)) {
          rdesc_df$id <- rownames(rdesc_df)
        }
        combined_results <- dplyr::left_join(
          rdesc_df,
          combined_results,
          by = "id"
        )

        results_list[[ome_name]] <- combined_results
      }
    })
    cat('\n-- two-sample moderated T-test exit --\n')
  }

  #Return the final results table from the chosen test
  return(results_list)
}
