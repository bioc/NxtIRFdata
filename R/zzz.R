#' NxtIRFdata: Data Package for NxtIRF
#'
#' This package contains files that provides a workable example for the 
#' NxtIRF package.\cr\cr
#' A synthetic reference, with genome sequence (FASTA) and gene annotation (GTF)
#' files are provided, based on the genes SRSF1, SRSF2, SRSF3, TRA2A, TRA2B, 
#' TP53 and NSUN5. These genes, with an additional 100 flanking nucleotides,
#' were used to construct an artificial "chromosome Z" (chrZ). 
#' Gene annotations,
#' based on release-94 of Ensembl GRCh38 (hg38), were modified with
#' genome coordinates corresponding to this artificial chromosome.\cr\cr
#' Accompanying this, an example dataset was created based on 6 samples from the
#' Leucegene dataset (GSE67039). Raw sequencing reads were downloaded from
#' [GSE67039](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE67039),
#' and were aligned to GRCh38 (Ensembl release-94) using STAR v2.7.3a. Then, 
#' alignments belonging to the 7 genes of the chrZ genome were filtered, and the
#' nucleotide sequences of these alignments were realigned to the chrZ reference
#' using STAR.\cr\cr
#' Additionally, NxtIRFdata contains Mappability exclusion regions generated
#' using NxtIRF, suitable for use in generating references based on hg38,
#' hg19, mm10 and mm9 genomes. These were generated empirically. Synthetic 70-nt
#' reads, with start distances 10-nt apart, were systematically generated from
#' the genome. These reads were aligned to the same genome using the STAR 
#' aligner. Then, the BAM file read coverage was assessed.
#' Whereas mappable regions are expected to be covered with 7 reads,
#' low mappability regions are defined as regions covered with 4 or fewer
#' reads.
#' @param path (Default = tempdir()) The desired destination path in which to 
#'   place a copy of the files. The directory does not need to exist but its 
#'   parent directory does.
#' @param overwrite (Default = `FALSE`)
#'   Whether or not to overwrite files if they already exist
#'   in the given path. 
#' @param offline (Default = `FALSE`)
#'   Whether or not to work in offline mode. This may be suitable
#'   if these functions have been previously run and the user wishes to run
#'   these functions without fetching online hub resources. Default = FALSE
#' @param genome_type Either one of `hg38`, `hg19`, 
#'   `mm10` or `mm9`
#' @param as_type (Default "GRanges") Whether to return the Mappability 
#'   exclusion data as a GRanges object `"GRanges"`, or a BED `"bed"` or gzipped
#'   BED `"bed.gz"` copied locally to the given directory `path`.
#' @return
#' For `chrZ_genome` and `chrZ_gtf`: returns the path to the example genome
#'   FASTA and gene annotation GTF files
#'
#' For `example_bams`: returns a vector specifying the location of the 6
#'   example BAM files, copied to the given `path` directory. Returns NULL if
#'   a connection to ExperimentHub could not be established, or if some BAM
#'   files could not be downloaded.
#'
#' For `get_mappability_exclusion`: returns the mappability exclusion regions
#'   resource, with type as specified by the parameter `as_type`. Returns NULL
#'   if a connection to ExperimentHub could not be established, or if the
#'   resource could not be downloaded.
#' @examples
#' # returns the location of the genome.fa file of the chrZ reference
#'
#' genome_path <- chrZ_genome() 
#'
#' # returns the location of the transcripts.gtf file of the chrZ reference
#'
#' gtf_path <- chrZ_gtf() 
#'
#' # Fetches data from ExperimentHub and places them in the given path
#' # returns the locations of the 6 example bam files
#'
#' bam_paths <- example_bams(path = tempdir()) 
#'
#' # Fetches data from AnnotationHub and places them in the given path
#' 
#' # returns the Mappability exclusion for hg38 directly as GRanges object
#'
#' hg38.MapExcl.gr <- get_mappability_exclusion(
#'     genome_type = "hg38", 
#'     as_type = "GRanges"
#' ) 
#' 
#' # returns the location of the Mappability exclusion gzipped BED for hg38
#'
#' gzippedBEDpath <- get_mappability_exclusion(
#'     genome_type = "hg38", 
#'     as_type = "bed.gz",
#'     path = tempdir()
#' ) 
#' 
#' # Getting NxtIRFdata directly from ExperimentHub
#'
#' require(ExperimentHub)
#' eh <- ExperimentHub()
#' NxtIRF_hub <- query(eh, "NxtIRF")
#' 
#' @references
#' Generation of the mappability files was performed using NxtIRF using
#' a method analogous to that described in:
#' 
#' Middleton R, Gao D, Thomas A, Singh B, Au A, Wong JJ, Bomane A, Cosson B, 
#' Eyras E, Rasko JE, Ritchie W.
#' IRFinder: assessing the impact of intron retention on mammalian gene 
#' expression. Genome Biol. 2017 Mar 15;18(1):51.
#' \url{https://doi.org/10.1186/s13059-017-1184-4}
#' @name NxtIRFdata-package
#' @aliases 
#' chrZ_genome
#' chrZ_gtf
#' example_bams
#' get_mappability_exclusion
#' @keywords package
#' @md
NULL

#' @describeIn NxtIRFdata-package Returns the location of the genome.fa file of 
#' the chrZ reference
#' @export
chrZ_genome <- function()
    system.file("extdata", "genome.fa", package="NxtIRFdata", mustWork=TRUE)

#' @describeIn NxtIRFdata-package Returns the location of the transcripts.gtf 
#' file of the chrZ reference
#' @export
chrZ_gtf <- function()
    system.file("extdata", "transcripts.gtf", 
        package="NxtIRFdata", mustWork=TRUE)

#' @describeIn NxtIRFdata-package Fetches data from ExperimentHub and places 
#' them in the given path; returns the locations of the 6 example bam files
#' @export
example_bams <- function(path = tempdir(), overwrite = FALSE, offline = FALSE)
{
    .find_and_create_dir(path)

    bam_samples <- c("02H003", "02H025", "02H026", "02H033", "02H043", "02H046")
    files_to_make <- sprintf(file.path(path, "%s.bam"), bam_samples)
    if(all(file.exists(files_to_make)) & !overwrite) return(files_to_make)

    titles = sprintf("NxtIRF/example_bam/%s", bam_samples)
    if(!any(is.na(.query_local_cache(titles)))) {
        hubobj <- NULL   # Avoid loading ExperimentHub if all files in cache
    } else {
        tryCatch({
            hubobj <- ExperimentHub::ExperimentHub(localHub = offline)
        }, error = function(e) {
            hubobj <- NULL
        })
        if(is.null(hubobj)) return(NULL)
    }
    
    files <- c()
    for(i in seq_len(length(titles))) {
        title <- titles[i]
        file_to_make <- files_to_make[i]
        if(!file.exists(file_to_make) | overwrite) {
            files = append(files, 
                .hub_to_bam(title, file_to_make, overwrite, offline, hubobj))        
        } else {
            files <- append(files, file_to_make)
        }
    }
    if(!all(file.exists(files))) {
        message("Some BAM files could not be found on ExperimentHub")
        return(NULL)
    }
    return(files)
}

#' @describeIn NxtIRFdata-package Fetches data from ExperimentHub and 
#' places a copy in the given path; 
#' returns the location of this Mappability exclusion 
#' BED file
#' @export
get_mappability_exclusion <- function(
        genome_type = c("hg38", "hg19", "mm10", "mm9"),
        as_type = c("GRanges", "bed", "bed.gz"),
        path = tempdir(), overwrite = FALSE, offline = FALSE
) {
    genome_type <- match.arg(genome_type)
    if(genome_type == "") 
        stop("genome_type must be one of `hg38`, `hg19`, `mm10`, or `mm9`")
    as_type <- match.arg(as_type)
    if(as_type == "") 
        stop("as_type must be one of `GRanges`, `bed`, or `bed.gz`")
    if(as_type != "GRanges") .find_and_create_dir(path)

    title <- paste("NxtIRF", "mappability", genome_type, sep="/")
    destfile <- sprintf(file.path(path, "%s.MappabilityExclusion.bed"),
        genome_type)
    if(file.exists(paste0(destfile, ".gz")) & !overwrite & as_type != "bed.gz") 
        return(paste0(destfile, ".gz"))
    if(file.exists(destfile) & !overwrite & as_type != "bed") return(destfile)
    
    hubobj <- NULL
    # Check cache:
    if(!is.na(.query_local_cache(title))) {
        gr <- readRDS(.query_local_cache(title))
    } else {
        tryCatch({
            hubobj <- ExperimentHub::ExperimentHub(localHub = offline)
        }, error = function(e) {
            hubobj <- NULL
        })
        if(is.null(hubobj)) {
            message(
                "Failed establishing ExperimentHub connection. ",
                "Run ExperimentHub::ExperimentHub() to reproduce error msg"
            )
            return(NULL)
        }
        record_name <- names(hubobj[hubobj$title == title])
        if(length(record_name) < 1) {
            stopmsg <- paste("Mappability record not found -", genome_type,
                ifelse(offline, "- Perhaps try again in `online` mode.",
                paste("- Ensure ExperimentHub package is",
                    "updated to the latest version")))
            stop(stopmsg)
        } else if(length(record_name) > 1) {
            stopmsg <- paste("Multiple mappability records found -", genome_type,
                "- please update NxtIRFdata to latest version")
            stop(stopmsg)
        }
        tryCatch({
            cache_loc <- ExperimentHub::cache(hubobj[hubobj$title == title])
        }, error = function(e) {
            cache_loc <- ""
        })
        if(!file.exists(cache_loc)) {
            message("Downloading mappability from ExperimentHub failed")
            return(NULL)
        }
        .add_file_to_local_cache(cache_loc, title)
        gr <- hubobj[[record_name]]  # GRanges object from Rds
    }
    
    if(as_type == "GRanges") return(gr)
    if(!file.exists(destfile) | overwrite) {
        if(file.exists(destfile)) file.remove(destfile)
        rtracklayer::export(gr, destfile, "bed")
    }
    if(!file.exists(destfile)) {
        message("rtracklayer BED export failed for - ", genome_type)
        return(NULL)
    }
    if(as_type == "bed") return(destfile)
    if(file.exists(paste0(destfile, ".gz")))
        file.remove(paste0(destfile, ".gz"))
    R.utils::gzip(destfile)
    return(paste0(destfile, ".gz"))
}

# internal use only
.find_and_create_dir <- function(path) {
    if(!file.exists(dirname(path)))
        stop("Cannot create directory for given path")
    if(!file.exists(path)) dir.create(path)
}

# Internal use only
.hub_to_bam <- function(
        title, destfile, overwrite = FALSE, offline = FALSE, hubobj
) {
    stopmsg <- cache_loc <- ""
    if(file.exists(destfile) & !overwrite) return(destfile)

    # Check local cache first:
    cache_loc <- .query_local_cache(title)
    if(!is.na(cache_loc)) {
        if(file.exists(destfile)) file.remove(destfile)
        file.copy(cache_loc, destfile)
        return(destfile)
    }

    record <- hubobj[grepl(title, hubobj$title)]
    if(length(record) > 1) {
        stopmsg <- paste("Multiple hub records exist -", title,
            "- please update NxtIRFdata to latest version")
        message(stopmsg)
        return("")
    } else if(length(record) == 0) {
        stopmsg <- paste("Hub record not found -", title,
            ifelse(offline, "- Perhaps try again in `online` mode.",
            paste("- Ensure ExperimentHub package is",
                "updated to the latest version")))
        message(stopmsg)
        return("")
    }
    if(!offline) {
        fetch_msg <- paste("Downloading record from hub, as required:", title)
        message(fetch_msg)    
    }
    tryCatch({
        cache_loc <- ExperimentHub::cache(record)[1] 
        # Files are in order BAM, BAI
    }, error = function(e) {
        cache_loc <- ""
    })
    if(file.exists(cache_loc)) {
        if(file.exists(destfile)) file.remove(destfile)
        file.copy(cache_loc, destfile)
        
        # Cache a local copy for faster access next time
        .add_file_to_local_cache(destfile, title)
        return(destfile)
    } else {
        stopmsg <- paste("Cache fetching failed -", title)
        message(stopmsg)
        return("")
    }
}

.query_single_record <- function(bfc, record_name, version = "1.0.0") {
    # returns rpath if version current, otherwise delete old record
    res = BiocFileCache::bfcquery(bfc, record_name)
    if(nrow(res) == 1) {
        if("version" %in% BiocFileCache::bfcmetalist(bfc)) {
            meta <- BiocFileCache::bfcmeta(bfc, "version")
            record_version <- meta$version[match(res$rid, meta$rid)]
            if(!is.na(record_version) && record_version == version) {
                return(res$rpath)
            } else {
                # delete old version and return NA
                BiocFileCache::bfcremove(bfc, res$rid)
                return(NA)
            }
        } else {
            BiocFileCache::bfcremove(bfc, res$rid)
            return(NA)
        }
    } else {
        return(NA)
    }
}

.query_local_cache <- function(record_name) {
    version <- "1.0.0"
    cache <- tools::R_user_dir(package = "NxtIRFdata", which="cache")
    bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
    ret <- c()
    for(record in record_name) {
        ret <- c(ret, .query_single_record(bfc, record, version))
    }
    return(ret)
}

.add_file_to_local_cache <- function(filename, record_name, overwrite = FALSE) {
    version <- "1.0.0"
    if(!file.exists(filename)) {
        stopmsg <- paste("Given file doesn't exist:", filename)
        stop(stopmsg, call. = FALSE)
    } 
    cache <- tools::R_user_dir(package = "NxtIRFdata", which="cache")
    bfc <- BiocFileCache::BiocFileCache(cache, ask = FALSE)
    res <- BiocFileCache::bfcquery(bfc, record_name)
    if(nrow(res) == 1) {
        if(!overwrite) return(res$rpath)
        BiocFileCache::bfcremove(bfc, res$rid)
    }
    rpath = BiocFileCache::bfcadd(bfc, record_name, filename)
    
    # Add version metadata
    res <- BiocFileCache::bfcquery(bfc, record_name)
    version_meta <- data.frame(rid = res$rid, version = version)
    BiocFileCache::bfcmeta(bfc, "version", append = TRUE) <- version_meta
    
    return(rpath)
}