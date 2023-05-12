################################################################################
# Module: MULTI-OME HEATMAP
################################################################################


################################################################################
# Shiny functions (server and UI)
################################################################################

## UI for heatmap tab
heatmapTabUI <- function(id = 'heatmapTab') {
  ns <- NS(id) # namespace function
  
  sidebarLayout(
    sidebarPanel(tagList(
      br(),
      
      ## text input
      selectizeInput(ns('genes'), 
                     label=paste('Enter your genes of interest (max. ',
                                 GENEMAX,')', sep=''),
                     choices = NULL,
                     multiple=T),
      
      br(),
      
      ## inputs to customize heatmap
      fluidRow(
        column(4, radioButtons(ns('zscore'), 
                               label='Z-score', 
                               choices=c('row', 'none'), 
                               selected='row')),
        
        column(4, 
               radioButtons(ns('PTMsites'), 
                            label='PTM sites', 
                            choices=c('most variable', 'all'), 
                            selected='most variable')),
        column(4,
               radioButtons(ns('show.sample.label'),
                            label = "Sample labels",
                            choices = c('show' = TRUE, 'hide' = FALSE),
                            selected = FALSE))
      ), # end fluidRow
      
      fluidRow(
        column(6, textInput(ns('min.val'), 
                            label='min', 
                            value=-2, 
                            width='80%')),
        column(6, textInput(ns('max.val'), 
                            label='max', 
                            value=2, 
                            width='80%'))
      ), #end fluidRow
      
      ## inputs for sorting
      fluidRow(
        column(12, selectizeInput(ns('sort.after'), 'Sort by', 
                                  choices = NULL,
                                  multiple=FALSE))  
      ), #end fluidRow
      
      ## inputs for order
      fluidRow(column(12, strong("Data order (drag and drop to re-order)"), br(), orderInput(
        label = "",
        items = list(),
        inputId = ns('ome.order'),
        class = "btn-group-vertical",
        width = '150px'
      ))),
      
      br(),
      
      ## download buttons
      fluidRow(column(6, downloadButton(ns('downloadHM'), 'Download PDF')),
               column(6, downloadButton(ns('downloadTab'), 'Download Excel'))),
      
      br(),
      
      ## Instructions
      p(strong("Getting started")),
      p(paste("Enter your gene names of interest (official gene symbols, e.g. EGFR) into the text field.",
              "You can enter up to", GENEMAX, "genes.")),
      br()
    )), # end tagList
    
    mainPanel(plotOutput(ns("HM"))))
}

heatmapTabServer <- function(id = 'heatmapTab') {
  moduleServer(
    id,
    ## module function
    function (input, output, session) {
      
      o <- preprocess_gct_files(GCTs)
      merged_mat <- o$merged_mat
      merged_rdesc <- o$merged_rdesc
      merged_cdesc <- o$merged_cdesc
      rm(o)
      
      # function to check if selected annotation columns are valid
      is_valid_anno <- function(col) {
        valid_categorical <- length(setdiff(unique(col), NA)) <= MAX_ANNO_LEVELS
        valid_numeric <- all(suppressWarnings(!is.na(as.numeric(as.character(setdiff(col, NA))))))
        return(valid_categorical | valid_numeric)
      }
      sample_anno <- merged_cdesc %>% select(Sample.ID | where(is_valid_anno))
      
      # update selectizeInput for genes
      updateSelectizeInput(session,
                           inputId = "genes",
                           choices = sort(unique(merged_rdesc$geneSymbol)),
                           server = T)
      
      # update selectTinput for annotations
      updateSelectInput(session,
                        inputId = "sort.after",
                        choices = setdiff(names(sample_anno), 'Sample.ID'))
      
      # update data ordering options
      updateOrderInput(session,
                       inputId = 'ome.order',
                       items = sort(unique(merged_rdesc$DataType)))
      
      ## get heatmap parameters
      params <- reactive({list(genes.char = input$genes,
                               zscore = input$zscore,
                               PTMsites = input$PTMsites,
                               min.val = as.numeric(input$min.val),
                               max.val = as.numeric(input$max.val),
                               sort.after = input$sort.after,
                               show.sample.label = input$show.sample.label,
                               custom_anno_colors = get0('custom_anno_colors'),
                               ome.order = input$ome.order)})
      
      out <- reactive({
        validate(
          need(params()$genes.char, "Input genes to see results"),
          need(params()$min.val < params()$max.val, "Input valid min and max")
        )
        
        myComplexHeatmap(merged_mat,
                         merged_rdesc,
                         sample_anno,
                         params = params())
      })
      
      
      ## download HM pdf
      output$downloadHM <- downloadHandler(
        filename = paste0(FILENAMESTRING, '_',
                          gsub(' |\\:','-', Sys.time()),
                          '.pdf'),
        content = function(file) {
          pdf(file = file,
              width = 1400/72,
              height = (dynamicHeightHM(nrow(out()$Table))+48)/72)
          draw(out()$HM, 
               annotation_legend_side='bottom')
          dev.off()
        }
      )
      
      ## download excel
      output$downloadTab <- downloadHandler(
        filename = paste0(FILENAMESTRING, '_',
                          gsub(' |\\:','-', Sys.time()),
                          '.xlsx'),
        content = function(file) {
          tab = out()$Table
          WriteXLS('tab',
                   ExcelFileName=file,
                   SheetNames=FILENAMESTRING,
                   FreezeRow = ncol(sample_anno),
                   FreezeCol=5,
                   row.names=T)
        }
      )
      
      
      ## render the heatmap
      
      plotHeight <- reactive({tryCatch({dynamicHeightHM(nrow(out()$Table))},
                                       error = function (cond) 'auto')})
      
      output$HM <- renderPlot({
        validate(
          need(params, "Complete setup steps to see heatmap.") %then%
            need(params()$genes.char, "Input genes to see results") %then%
            need(params()$min.val < params()$max.val, "Input valid min and max") %then%
            # need(HM$params()$sort.after %in% names(sample_anno) || dim(sample_anno)[2] == 1,
            #      "Invalid annotation selection") %then%
            need(out, "Heatmap not yet generated."))
        
        
        draw(out()$HM, annotation_legend_side='bottom')
      }, height = plotHeight)
    }) # end moduleServer
}


################################################################################
# Helper functions
################################################################################

## preprocess and merge GCT files
preprocess_gct_files <- function(gct_parsed, 
                                 setup_inputs = list(labels = names(gct_parsed),
                                                     datatypes = rep('SpectrumMill', length(gct_parsed)),
                                                     geneSymbolColumns = rep(NULL, length(gct_parsed)),
                                                     is_VMs = rep(NULL, length(gct_parsed)),
                                                     VM_columns = rep(NULL, length(gct_parsed)))) {
  
  # cat("\nPreprocessing gct files:", gct_files, "\n", sep = '\n')
  
  # extract setup inputs
  labels <- setup_inputs$labels
  datatypes <- setup_inputs$datatypes
  geneSymbol_columns <- setup_inputs$geneSymbol_columns
  is_VMs <- setup_inputs$is_VMs
  VM_columns <- setup_inputs$VM_columns
  # 
  # ## parse all gct files
  # gct_parsed <- lapply(gct_files, function(f) parse_gctx(f))
  # 
  # # show progress in progress bar
  # if (shiny::isRunning()) incProgress()
  
  ## add data label and ID to gct
  gct_parsed <- lapply(1:length(gct_parsed), function(i) {
    # get gct object, data label, and geneSymbol column name
    gct <- gct_parsed[[i]]
    label <- labels[i]
    datatype <- datatypes[i]
    geneSymbol_column <- geneSymbol_columns[i]
    is_VM <- is_VMs[i]
    VM_column <- VM_columns[i]
    
    # get default gene symbol column for SpectrumMill
    if (datatype == 'SpectrumMill') {
      geneSymbol_column <- 'geneSymbol'
    }
    
    # condense rdesc to only necessary information
    rdesc <- meta(gct, dim='row')
    rdesc_new <- data.frame(ID = paste0(gct@rid, '.', i),
                            geneSymbol = rdesc[[geneSymbol_column]],
                            DataType = rep(label, dim(rdesc)[1]))
    rownames(rdesc_new) <- rdesc_new$ID
    
    ## add info for VM sites
    # for SpectrumMill data
    if (datatype == 'SpectrumMill') {
      # search for VM name regexp in ID's, if its not there then its not a VM
      is_VM <- any(grepl('_[[:upper:]][[:alnum:]]+[[:lower:]]_', gct@rid))
      rdesc_new$is_VM <- rep(is_VM, dim(rdesc)[1])
      
      if (is_VM) {
        rdesc_new$VM_name <- sapply(strsplit(gct@rid, split = '_'), 
                                    function(x) x[grepl('^[[:upper:]][[:alnum:]]+[[:lower:]]', 
                                                        gsub(' ', '', x))])
      } else {
        rdesc_new$VM_name <- rep('', dim(rdesc)[1])
      }
      
      # for non-SpectrumMill data
    } else {
      rdesc_new$is_VM <- rep(is_VM, dim(rdesc)[1])
      
      # get VM names
      if(is_VM) {
        if (length(intersect(VM_column, names(rdesc))) == 0) {
          warning(paste(VM_column, "not found in GCT rdesc"))
        }
        rdesc_new$VM_name <- rdesc[[VM_column]]
      } else {
        rdesc_new$VM_name <- rep('', dim(rdesc)[1])
      }
    }
    
    # make sure Sample.ID is in cdesc
    cdesc <- meta(gct, dim='column')
    cdesc$Sample.ID <- gct@cid
    
    # change gct mat rownames to match rdesc_new rownames
    gct_mat <- mat(gct)
    rownames(gct_mat) <- paste0(rownames(gct_mat), '.', i)
    
    # create new GCT with updated rdesc
    gct_new <- new('GCT', 
                   mat = gct_mat, 
                   rdesc = rdesc_new,
                   cdesc = cdesc)
    
    # show progress in progress bar
    # if (shiny::isRunning()) incProgress()
    
    return(gct_new)
  })
  
  ## merge all gct's into one large one
  merged_gct <- Reduce(function(gct1, gct2) merge_gct(gct1, gct2, dim='row'), gct_parsed)
  
  # show progress in progress bar
  # if (shiny::isRunning()) incProgress()
  
  
  
  return(list(merged_mat = merged_gct@mat,
              merged_rdesc = merged_gct@rdesc,
              merged_cdesc = merged_gct@cdesc))
}

## make complex heatmap
## calls global variables merged_rdesc, merged_mat, sample_anno
myComplexHeatmap <- function(merged_mat,
                             merged_rdesc,
                             sample_anno,
                             params) {
  
  # extract parameters
  genes.char <- params$genes.char
  zscore <- params$zscore
  PTMsites <- params$PTMsites
  min.val <- params$min.val
  max.val <- params$max.val
  sort.after <- params$sort.after
  show.sample.label <- params$show.sample.label
  custom_anno_colors <- params$custom_anno_colors
  ome.order <- params$ome.order
  
  # extract genes
  genes.all <- extractGenes(genes.char, select(merged_rdesc, geneSymbol))
  genes.vec <- genes.all$genes.vec
  
  # extract rows for that gene
  genes.Table <- as.data.frame(merged_mat)[which(merged_rdesc$geneSymbol %in% genes.vec), ]
  row.anno <- merged_rdesc[which(merged_rdesc$geneSymbol %in% genes.vec), ]
  
  # generate heatmap table
  # first 4 columns are: geneSymbol, ome, row_label, is_VM
  # the rest of the columns are labeled by each sample, contain abundance data
  # NOTE: if you ever edit this so that more/less then 4 columns are 
  # description-type information, check to other lines where genes.Table is 
  # indexed to make sure unexpected errors don't occur
  genes.Table <- getHMTable(genes.Table, row.anno, params) 
  genes.Table <- genes.Table[ ,c(1:4, order(names(genes.Table)[-(1:4)])+4)]
  
  # re-order the omes
  genes.Table$ome <- factor(genes.Table$ome, ome.order)
  
  # use only most variable VM sites (if requested)
  if (PTMsites == "most variable" && any(genes.Table$is_VM)) {
    genes.Table.VM <- genes.Table[genes.Table$is_VM, ]
    
    # calculate SD for all PTM sites
    genes.Table.VM$SD <-
      apply(genes.Table.VM[, -(1:4)], 1, function(x) sd(x, na.rm = TRUE))
    
    # select the maximum SD for each ome/gemeSymbol
    genes.Table.VM <- genes.Table.VM %>%
      group_by(ome, geneSymbol, .drop = TRUE) %>%
      filter(SD == max(SD)) %>%
      select(-SD) %>%
      ungroup()
    
    # add back to original table
    genes.Table <- rbind(genes.Table[!genes.Table$is_VM, ], genes.Table.VM)
    genes.Table <- as.data.frame(genes.Table)
    
    # get rownames back
    rownames(genes.Table) <- make.unique(genes.Table$row_label)
  }
  
  # make matrix to feed into heatmap
  # z-score rows if requested
  if (zscore == "row") {
    genes.Matrix <- t(apply(genes.Table[,-(1:4)], 1,
                            function(x)(x-mean(x, na.rm=T))/sd(x, na.rm=T)))
  } else {
    genes.Matrix <- as.matrix(genes.Table[,-(1:4)])
  }
  rownames(genes.Matrix) <- make.unique(genes.Table$row_label)
  
  
  ## make heatmap for annotations
  anno.fig <- sample_anno[order(sample_anno$Sample.ID), , drop = FALSE]
  rownames(anno.fig) <- anno.fig$Sample.ID
  anno.fig <- anno.fig[, -(1), drop = FALSE]
  
  if (dim(anno.fig)[2] == 0) {
    HM.anno <- NULL
    column.to.sort <- NULL
    final.Table <- genes.Table
    
  } else {
    # convert numeric columns in anno.fig to numeric type
    numeric_cols <- sapply(anno.fig, function(col) 
      all(suppressWarnings(!is.na(as.numeric(as.character(col[!is.na(col)]))))))
    anno.fig[, numeric_cols] <- sapply(anno.fig[, numeric_cols], as.numeric)
    
    # make custom color palette
    set.seed(1)
    anno.fig.color <- lapply(anno.fig, myColorPalette)
    for (anno in names(custom_anno_colors)) {
      anno.fig.color[[anno]] <- custom_anno_colors[[anno]]
    }
    
    # combine annotation table with genes.Table for final output
    anno.fig.new <- data.frame(matrix(ncol=ncol(genes.Table),
                                      nrow=ncol(anno.fig)))
    names(anno.fig.new) <- names(genes.Table)
    rownames(anno.fig.new) <- names(anno.fig)
    anno.fig.new[, -(1:4)] <- t(anno.fig)
    final.Table <- rbind(anno.fig.new, genes.Table)
    
    HM.anno <- HeatmapAnnotation(df = anno.fig,
                                 col = anno.fig.color,
                                 show_legend = T,
                                 show_annotation_name = T,
                                 annotation_name_side = "left",
                                 na_col = "grey",
                                 annotation_legend_param = list(
                                   direction = 'horizontal'),
                                 height = unit(0.5, 'cm') * nrow(anno.fig))
    column.to.sort <- anno.fig[, sort.after]
  }
  
  # define color map for heatmap
  col_fun_ratios <- colorRamp2(c(min.val, 0, max.val), 
                               c("blue", "white", "red"))
  
  # make final heatmap for genes
  HM <- Heatmap(genes.Matrix,
                col = col_fun_ratios,
                column_title = "Sample",
                row_title_rot = 0,
                row_order = order(genes.Table$ome, genes.Table$row_label),
                cluster_rows = F,
                cluster_columns = F,
                row_split = genes.Table$geneSymbol,
                column_split = column.to.sort, 
                top_annotation = HM.anno,
                show_row_names = T,
                show_column_names = show.sample.label,
                name = 'relative abundance',
                height = unit(0.5, 'cm') * nrow(genes.Matrix),
                column_names_side = "top",
                column_names_rot = 45,
                column_names_gp = gpar(fontsize = 10),
                column_gap = if (is.numeric(column.to.sort)) {unit(0, "mm")} else {unit(1, "mm")},
                heatmap_legend_param = list(title_position = "lefttop-rot",
                                            legend_height = unit(3.3, "cm")))
  
  return(list(HM=HM, Table=final.Table))
}

## function to extract gene names from a string input
extractGenes <- function(genes.char, table){
  
  if(is.null(genes.char))
    return(NULL)
  
  if( length(genes.char) == 0 ){
    return(NULL)
  }
  ## extract genes
  genes.vec= unlist(strsplit(genes.char, ','))
  if(length(genes.vec)==1)
    genes.vec=unlist(strsplit(genes.char, ' '))
  if(length(genes.vec)==1)
    genes.vec=unlist(strsplit(genes.char, ';'))
  
  ## unique gene names
  genes.vec <- unique(genes.vec)
  
  ## limit to 'GENEMAX' genes
  if(length(genes.vec) > GENEMAX){
    warning(paste('more than', GENEMAX,'gene ids submitted! Showing results for the first 20 genes in the list.\n'))
    genes.vec <- genes.vec[1:GENEMAX]
  }
  
  ## remove spaces
  genes.vec <- sapply(genes.vec, function(x) gsub(' ', '', x))
  
  ## exclude genes that are not in the dataset
  genes.notInTable <- setdiff(genes.vec, table$geneSymbol)
  if (length(genes.notInTable) > 0) {
    warning("Some genes were not found in the dataset. These are being excluded.\n")
    genes.vec <- setdiff(genes.vec, genes.notInTable)
  }
  
  return(list(genes.vec=genes.vec, genes.notInTable=genes.notInTable))
}

## function to get the heatmap table
getHMTable <- function(genes.Table, row.anno, params) {
  # make row labels
  row_labels <- paste(row.anno$geneSymbol, row.anno$DataType, row.anno$VM_name)
  
  # get omes
  ome <- factor(row.anno$DataType)
  
  # combine into one genes.Table
  genes.Table <- cbind(data.frame(geneSymbol = row.anno$geneSymbol,
                                  ome = ome,
                                  row_label = row_labels,
                                  is_VM = row.anno$is_VM),
                       genes.Table)
  rownames(genes.Table) <- make.unique(genes.Table$row_label, sep= ' ')
  
  return (genes.Table)
}

# helper function for some of my "validate()" statements
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

## function to dynamically determine the height (in px) of the heatmap
## depending on the number of genes
dynamicHeightHM <- function(n.entries){
  height <- 0.3*(n.entries+12) + 3  ## height in inch
  height <- height * 48             ## inch  to pixel
  
  return(height)
}

## function to make prettier color palettes
myColorPalette <- function(anno) {
  
  anno.unique <- setdiff(unique(anno), NA)
  
  if (length(anno.unique) == 0) {
    return(c('NA' = 'grey'))
  }
  
  else if (length(anno.unique) <= 2) {
    # check if its a binary 0/1 +/- type of annotation
    if (length(setdiff(as.character(anno.unique), c('0', '1'))) == 0) {
      col <- c('0'='grey90', '1'='black')
      col <- col[as.character(anno.unique)]
      
    } else if (length(setdiff(anno.unique, c('Positive', 'Negative'))) == 0) {
      col <- c('Positive'='grey90', 'Negative'='black')
      col <- col[anno.unique]
    }
    
    else {
      # generate a color pair from RColorBrewer
      i <- sample(c(1, 3, 5, 7, 9, 11), 1)
      col <- brewer.pal(12, "Paired")[i:(i+length(anno.unique) - 1)]
      names(col) <- anno.unique
    }
    
    # check if its a numeric type
  } else if (all(is.numeric(anno.unique))) {
    
    # check if there's less than 5 values, this probably means the annotation is
    # not supposed to be a continuous annotation
    if (length(anno.unique) <= 5) {
      col <- sample(brewer.pal(12, "Set3")[c(1:8,10:12)], length(anno.unique))
      names(col) <- anno.unique
      
      # otherwise, make a continuous color palette
    } else {
      palette_name <- sample(c("Purples", "Oranges", "Greens", "Blues", "Reds"), 1)
      min_anno <- min(anno.unique)
      max_anno <- max(anno.unique)
      
      col <- colorRamp2(c(min_anno, mean(min_anno, max_anno), max_anno),
                        brewer.pal(3, palette_name))
    }
    
    # otherwise, this is a categorical annotation with > 2 levels
    # picks colors from RColorBrewer's Set2 palette
  } else {
    col <- colorRampPalette(brewer.pal(7, "Set2"))(length(anno.unique))
    names(col) <- anno.unique
  }
  
  return(col)
}
