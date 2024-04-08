## COUNT_TBL ##
count_tbl <- function(df, var1, var2, var3, fi) {
  
  gene <- df[var1] %>% unique()
  id <- df[var2] %>% unique()
  
  rowname <- unlist(gene)
  colname <- unlist(id)
  
  res <- matrix(rep(NA, length(rowname) * length(colname)), nrow = length(rowname), 
                ncol = length(colname)) %>% 
    `rownames<-`(rowname) %>% 
    `colnames<-`(colname)
  
  for (x in colname){
    for (y in rowname) {
      
      df1 <- filter(df, get(var1) == y & get(var2) == x & `FUNCTIONAL-IMPACT` == fi)
      
      res[df1[[var1]], df1[[var2]]] <- df1[[var3]]
      
    }
  }
  
  return(res)
  
}

## EXTRA_ID ##

extra_id <- function(mat, si) {
  if (any(!(si$`FMI.SAMPLE.ID` %in% colnames(mat)))){
    extra <- si$FMI.SAMPLE.ID[!(si$FMI.SAMPLE.ID %in% colnames(mat))]
    
    rowname <- rownames(mat)
    colname <- extra
    
    res <- matrix(rep(NA, length(rowname) * length(colname)), nrow = length(rowname), 
                  ncol = length(colname)) %>% 
      `rownames<-`(rowname) %>% 
      `colnames<-`(colname)
    
    cbind(mat, res)
  } else {
    mat
  }

}

## Get sample ID?
# samp <- function(obj) {
#   data.frame("SUBJECT-ID" = obj@colData$subject_id,
#              "FMI.SAMPLE.ID" = obj@colData$vendor_sample_id)
# }

# ## Defining basic.report? 
# #create class
# basic.report <- function(x) structure(x, class = c("basic.report", "list"))
# 
# #head format???
# head.basic.report <- function(obj){
#   a <- obj$Short.Variants$`SV-PROTEIN-CHANGE`$known
#   fi <- "known"
#   nr <- nrow(a)
#   nc <- ncol(a)
#   
#   if(nr > 5) {
#     if(nc > 5) {
#       y = a[1:5, 1:5]
#       ecol = paste0("... ", nc - 5, " more column(s)")
#       y = as.data.frame(cbind(y, rep("...", 5)))
#       colnames(y) = c(colnames(y)[-6], ecol)
#       print(y) 
#       cat("...", nr-5, "more row(s)\n")
#     } else {
#       y = as.data.frame(a[1:5,],drop.=FALSE)
#       print(y)
#       cat("...", nr-5, "more row(s)\n")
#     }
#   } else {
#     if(nc > 5) {
#       y = a[,1:5,drop=FALSE]
#       ecol = paste0("... ", nc-5, "more column(s)")
#       y = as.data.frame(cbind(y, rep("...", nr)))
#       colnames(y) = c(colnames(y)[-6], ecol)
#       print(y)
#     }else{
#       y = as.data.frame(a)
#       print(y)
#     }
#   }
#   
#   cat("===================================================\n")
#   cat("Variable: SV-PROTEIN-CHANGE, functional impact:", fi)
#   cat("===================================================\n")
#   cat("This is a snippet of a basic.report object, it contains more variables",
#       "\n than snown above.")
# }
# 
# # #dimensions of basic report....?????????
# # dim.basic.report <- function(obj){
# #   a <- obj$Short.Variants$`SV-PROTEIN-CHANGE`$known
# #   dim(a)
# # }
# 
# #print format???
# print.basic.report <- function(obj) {
#   
#   yn <- names(obj)[names(obj) %in% c("SampleInfo", "Short.Variants",
#                                      "Copy.Number.Alterations", "Rearrangements")]
#   lyn <- yn[yn %in% c("Short.Variants", "Copy.Number.Alterations", "Rearrangements")]
#   
#   n <- length(yn)
#   
#   con <- data.frame(
#     index = c(1:n),
#     names = yn,
#     class = sapply(yn, function(z) {
#       class(obj[[z]])
#     }),
#     dims = sapply(yn, function(y) {
#       if(class(obj[[y]]) == "data.frame"){
#         paste(dim(obj[[y]]),collapse = ",")
#       }
#       else if (class(obj[[y]]) == "list"){
#         paste(dim(obj[[y]][[1]][[1]]), collapse = ",")
#       }
#     }),
#     len = sapply(yn, function(y){
#       if(class(obj[[y]]) == "data.frame"){
#         NA
#       } else if (class(obj[[y]]) == "list"){
#         length(obj[[y]])
#       }
#     })
#   )
#   
#   #list item dimensions
#   lis <- sapply(lyn, simplify = F, function(z) {
#     
#     lapply(names(obj[[z]]), function(i) {
#       
#       len <- length(obj[[z]][[i]])
#       
#     }) %>% 
#       do.call(rbind, .) %>% 
#       as.data.frame() %>% 
#       `colnames<-`("length") %>% 
#       mutate("name" = names(obj[[z]]),
#              "class" = class(obj[[z]]))
#     
#   })
#   
#   #sub list item dimensions
#   lst_dims <-sapply(lyn, function(z){
#     
#     sapply(names(obj[[z]]), function(y) {
#       
#       sapply(1:length(obj[[z]][[y]]), function(i){
#         
#         paste(dim(obj[[z]][[y]][[i]]), collapse = ",")
#         
#       })
#       
#     }) %>% data.frame() %>% 
#       filter(row_number() == 1)
#     
#   }) 
#   
#   #writing lines
#   cat(paste("Contains", n, "major components: \n"))
#   cat("=============================== \n")
#   
#   lines <- lapply(yn, function(x) {
#     a <- dplyr::filter(con, names == x)
#     
#     line1 <- paste(paste0(a$index, "."), a$names, "with class", a$class, "(dims:", paste0(a$dims, "): \n"))
#     
#     if(a$class == "list") {
#       
#       b <- lis[[x]]
#       c <- lst_dims[[x]]
#       
#       line2 <- paste("  List contains", a$len, "elements: \n")
#       line3 <- paste(paste0("\t", b$name, ":"), b$class, "of", b$length, "\n")
#       
#       cat(line1, line2, line3, "\n")
#     } else {
#       cat(line1, "\n")
#     }
#     
#   })
#   
# }#end print.basic.report

## Make Main ##
make_main <- function(object) { 
  
  if(class(object) == "HeliosData") {
    
    x <- object@ExperimentList$copy_number
    y <- object@ExperimentList$short_variant
    z <- object@ExperimentList$rearrangement
    
    na_list <- c(!is.null(y), !is.null(x), !is.null(z))
    
    nl <- c("y", "x", "z")[na_list]
    
    #SampleInfo
    # SampleInfo <- sapply(nl, USE.NAMES = T, simplify = F, function(i) {
    #   samp(get(i))
    # }) %>%
    #   bind_rows() %>% 
    #   unique()
    # 
    
    SampleInfo <- attributes(object)$SAMP_INFO
    rownames(SampleInfo) <- SampleInfo$FMI.SAMPLE.ID
    
    SampleInfo$FMI.SAMPLE.ID <- as.factor(SampleInfo$FMI.SAMPLE.ID)
    SampleInfo$SUBJECT.ID <- as.factor(SampleInfo$SUBJECT.ID)
    
    
    #Pulling out Baitsets
    bs <- sapply(nl, USE.NAMES = T, simplify = T, function(i){
      
      get(i)@metadata$baitset_label
      
    }) %>% unique()
    
    gene_list <- "data/FMI_gene_lists_updated_8_22_22.csv"
    
    gene_data <- read.csv(gene_list, header = TRUE, sep = ";")
    baitsets <- t(gene_data)[-c(1:2), -4] %>%
      `colnames<-`(c("Baitset", "Use Case", "Region", t(gene_data)[2, 5:nrow(gene_data)] )) %>% 
      as.data.frame()
    
    exons <- baitsets %>% filter(Region == "EXON" & `Use Case` == "Clinical Report")
    gene_types <- colnames(baitsets)[-c(1:3)]
    
    ## baitset specific
    tf_genes <- c(exons %>% filter(Baitset == toupper(bs)) %>% unlist() %>% unname())[-c(1:3)]
    inc_genes <- ifelse(tf_genes == "1", TRUE, FALSE)
    
    in_set <- gene_types[inc_genes] #genes included in that baitset
    
    ## SV data ##
    if("y" %in% nl) {
      
      sv_genes_all <- y@rowRanges@elementMetadata[,c("gene")] %>% as.data.frame()
      sv_genes <- filter(sv_genes_all, sv_genes_all$. %in% gene_types)
      sv_alien <- filter(sv_genes_all, !(sv_genes_all$. %in% gene_types))
      
      sv_ref <- y@rowRanges@elementMetadata[,c("gene", "FMI.SAMPLE.ID", "functional_impact")] %>% as.data.frame()
      
      sv_vars <- c("SV-GENOME-POSITION", "SV-CDS-CHANGE", "SV-PROTEIN-CHANGE", "SV-PERCENT-READS", "SV-COVERAGE")
      
      opt_sv_vars <- c("TRANSCRIPT", "SV-REF", "SV-ALT")
      
      #pulling out percent reads and coverage
      reads <- y@metadata$numbers[, c("percent_reads", "total_reads")] %>% as.data.frame()
      change <- y@rowRanges@elementMetadata[, c("cds_change", "protein_change")] %>% as.data.frame()
      gene_position_sv <- paste0(rep(y@rowRanges@seqnames@values, y@rowRanges@seqnames@lengths), ":", y@rowRanges@ranges@start) 
      sv_opt <- y@rowRanges@elementMetadata[, c("ref_transcript", "ref_allele", "alt_allele")] %>% as.data.frame()
      
      sv_df <- cbind(sv_ref, "gene_position" = gene_position_sv, change, reads, sv_opt) %>% 
        `colnames<-`(c("GENE", "FMI.SAMPLE.ID", "FUNCTIONAL-IMPACT", sv_vars, opt_sv_vars))
      
      sv_impact_levels <- levels(sv_df$`FUNCTIONAL-IMPACT`) %>% `names<-`(c(levels(sv_df$`FUNCTIONAL-IMPACT`)))
      
      #making lists of lists
      Short.Variants <- sapply(sv_vars, USE.NAMES = T, simplify = F, function(x) { #make list with names matching colnames
        
        sapply(sv_impact_levels, USE.NAMES = T, simplify = F, function(y) {
          
          count_tbl(sv_df , "GENE", "FMI.SAMPLE.ID", x, y) %>% extra_id(SampleInfo)
          
        })
        
      }) 
      
      SV <- sapply(names(Short.Variants), USE.NAMES = T, simplify = F, function(x) {
        sapply(sv_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Short.Variants[[x]][[y]][rownames(Short.Variants[[x]][[y]]) %>% unname() %in% sv_genes$.,] 
        })
      })
      
      SV.Aliens <- sapply(names(Short.Variants), USE.NAMES = T, simplify = F, function(x) {
        sapply(sv_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Short.Variants[[x]][[y]][rownames(Short.Variants[[x]][[y]]) %>% unname() %in% sv_alien$.,] 
        })
      })
      
    }else {
      SV <- NULL
      sv_vars <- NULL
      sv_impact_levels <- NULL
      SV.Aliens <- NULL
    }#end if statement
    
    
    ## CNA data ##
    
    if("x" %in% nl){
      
      cna_genes_all <- x@rowRanges@elementMetadata[,c("gene")] %>% as.data.frame()
      cna_genes <- filter(cna_genes_all, cna_genes_all$. %in% gene_types)
      cna_alien <- filter(cna_genes_all, !(cna_genes_all$. %in% gene_types))
      
      cna_ref <- x@rowRanges@elementMetadata[,c("gene", "FMI.SAMPLE.ID", "functional_impact")] %>% as.data.frame()
      
      cna_vars <- c("CNA-POS", "CNA-TYPE" , "CNA-EXONS", "CNA-RATIO", "CNA-COPY-NUMBER")
      
      # pulling out the variables
      copies <- x@metadata$numbers[, c("ratio", "copy_number")] %>% as.data.frame()
      type <- x@rowRanges@elementMetadata[,c("type", "exons")]  %>%  as.data.frame() %>% 
        mutate_if(is.factor, as.character)
      gene_position_cna <- paste0(rep(x@rowRanges@seqnames@values, x@rowRanges@seqnames@lengths), ":", x@rowRanges@ranges@start,
                                  "-", x@rowRanges@ranges@start + x@rowRanges@ranges@width - 1)
      
      cna_df <- cbind(cna_ref, "gene_position" = gene_position_cna, type, copies) %>% 
        `colnames<-`(c("GENE", "FMI.SAMPLE.ID", "FUNCTIONAL-IMPACT", cna_vars))
      
      cna_impact_levels <- levels(cna_df$`FUNCTIONAL-IMPACT`) %>% `names<-`(c(levels(cna_df$`FUNCTIONAL-IMPACT`)))
      
      #making lists of lists 
      
      Copy.Number.Alterations <- sapply(cna_vars, USE.NAMES = T, simplify = F, function(x) { #make list with names matching colnames
        
        sapply(cna_impact_levels, USE.NAMES = T, simplify = F, function(y) {
          
          count_tbl(cna_df , "GENE", "FMI.SAMPLE.ID", x, y) %>% extra_id(SampleInfo)
          
        })
        
      })  
      
      CNA <- sapply(names(Copy.Number.Alterations ), USE.NAMES = T, simplify = F, function(x) {
        sapply(cna_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Copy.Number.Alterations[[x]][[y]][rownames(Copy.Number.Alterations[[x]][[y]]) %>% unname() %in% cna_genes$.,] 
        })
      })
      
      CNA.Aliens <- sapply(names(Copy.Number.Alterations ), USE.NAMES = T, simplify = F, function(x) {
        sapply(cna_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Copy.Number.Alterations[[x]][[y]][rownames(Copy.Number.Alterations[[x]][[y]]) %>% unname() %in% cna_alien$.,] 
        })
      })
      
    }else {
      CNA <- NULL
      cna_vars <- NULL
      cna_impact_levels <- NULL
      CNA.Aliens <- NULL
    }#end if statement
    
    
    ## RA data ##
    
    if("z" %in% nl){
      
      ra_genes_all <- z@rowRanges@elementMetadata[,c("gene")] %>% as.data.frame()
      ra_genes <- filter(ra_genes_all, ra_genes_all$. %in% gene_types)
      ra_alien <- filter(ra_genes_all, !(ra_genes_all$. %in% gene_types))
      
      ra_ref <- z@rowRanges@elementMetadata[,c("gene", "FMI.SAMPLE.ID", "functional_impact")] %>% as.data.frame()
      
      ra_vars <- c("REARR-POS1", "REARR-GENE1", "REARR-DESCRIPTION", "REARR-IN-FRAME?", 
                   "REARR-GENE2",  "REARR-POS2", "REARR-NUMBER-OF-READS")
      
      #pulling out percent reads and coverage
      reads <- z@metadata$numbers[, c("reads")] %>% as.data.frame()
      
      change <- z@rowRanges@elementMetadata[, c("gene", "type", "is_in_frame", "other_gene")] %>% as.data.frame()  %>% 
        mutate_if(is.factor, as.character)
      
      gene_position_ra1 <- paste0(rep(z@rowRanges@seqnames@values, z@rowRanges@seqnames@lengths), ":", 
                                  z@rowRanges@ranges@start,"-", 
                                  z@rowRanges@ranges@start + z@rowRanges@ranges@width - 1) 
      
      gene_position_ra2 <- paste0(rep(z@rowRanges@elementMetadata$other_range@seqnames@values,
                                      z@rowRanges@elementMetadata$other_range@seqnames@lengths), ":", 
                                  z@rowRanges@elementMetadata$other_range@ranges@start,"-",
                                  z@rowRanges@elementMetadata$other_range@ranges@start +
                                    z@rowRanges@elementMetadata$other_range@ranges@width - 1)
      
      ra_df <- cbind(ra_ref, "gene_position1" = gene_position_ra1, change,
                     "gene_position2" = gene_position_ra2, reads) %>% 
        `colnames<-`(c("GENE", "FMI.SAMPLE.ID", "FUNCTIONAL-IMPACT", ra_vars))
      
      ra_impact_levels <- levels(ra_df$`FUNCTIONAL-IMPACT`) %>% `names<-`(c(levels(ra_df$`FUNCTIONAL-IMPACT`)))
      
      #making lists of lists
      Rearrangements <- sapply(ra_vars, USE.NAMES = T, simplify = F, function(x) { #make list with names matching colnames
        
        sapply(ra_impact_levels, USE.NAMES = T, simplify = F, function(y) {
          
          count_tbl(ra_df , "GENE", "FMI.SAMPLE.ID", x, y) %>% 
            extra_id(SampleInfo)
          
        })
        
      }) 
      
      RA <- sapply(names(Rearrangements ), USE.NAMES = T, simplify = F, function(x) {
        sapply(ra_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Rearrangements[[x]][[y]][rownames(Rearrangements[[x]][[y]]) %>% unname() %in% ra_genes$.,] 
        })
      })
      
      RA.Aliens <- sapply(names(Rearrangements ), USE.NAMES = T, simplify = F, function(x) {
        sapply(ra_impact_levels, USE.NAMES = T, simplify = F, function(y){
          Rearrangements[[x]][[y]][rownames(Rearrangements[[x]][[y]]) %>% unname() %in% ra_alien$.,] 
        })
      })
      
      
    } else {
      RA <- NULL
      ra_vars <- NULL
      ra_impact_levels <- NULL
      RA.Aliens <- NULL
    }#end if statement
    
    
    ### FINAL OBJECT ### 
    
    res1 <- list(
      "Short.Variants" = SV, 
      "Copy.Number.Alterations" = CNA, 
      "Rearrangements" = RA, 
      "sv.variables" = sv_vars, 
      "cna.variables" = cna_vars, 
      "ra.variables" = ra_vars,
      "sv.FI" = unname(sv_impact_levels),
      "cna.FI" = unname(cna_impact_levels),
      "ra.FI" = unname(ra_impact_levels))[rep(na_list, 3)]
    
    
    alien_genes <- list("SV.Aliens" = SV.Aliens,
                        "CNA.Aliens" = CNA.Aliens,
                        "RA.Aliens" = RA.Aliens)[na_list]
    
    sample_inf <- list("SampleInfo" = SampleInfo)
    
    res <- c(sample_inf,
             res1,
             alien_genes, 
             "baitset" = c(bs))
    
    class(res) = "basic.report"
    
    return(res)
    
  } else {
    msg <- "Input must be of class 'HeliosData'"
    stop(msg)
  }
} #end function

# ## Example: ##
# 
# oapl_file <- system.file(
#   "extdata/FMI",
#   "Example_OAPL_PLUS_07MAY2021.xlsx",
#   package = "helios"
# )
# ospl_file <- system.file(
#   "extdata/FMI",
#   "Example_OSPL__29APR2021_STANDARD_TEST.xlsx",
#   package = "helios"
# )
# 
# read_data(oapl_file, ospl_file, "DX1") %>% make_main()
