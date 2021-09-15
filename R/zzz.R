#' @importFrom rtracklayer export
#' @importFrom AnnotationHub cache AnnotationHub
#' @importFrom ExperimentHub ExperimentHub
NULL

#' NxtIRFdata: Data Package for NxtIRF
#'
#' This package contains files that provides a workable example for the 
#' NxtIRF package.\cr\cr
#' A mock reference, with genome sequence (FASTA) and gene annotation (GTF)
#' files are provided, based on the genes SRSF1, SRSF2, SRSF3, TRA2A, TRA2B, 
#' TP53 and NSUN5, of which sequences are used to construct an artificial 
#' chromosome Z. This was generated based on release-94 of Ensembl GRCh38 (hg38)
#' reference.\cr\cr
#' NxtIRFdata contains 6 example bam files based on samples from the 
#' Leucegene dataset (GSE67039). Bam files are constructed
#' based on the complete bam files of 6 samples from Leucegene,
#' subsetted by regions containing the 7 above genes. Then, the reads of these 
#' subsetted BAMs were realigned to the mock reference using STAR.\cr\cr
#' Additionally, NxtIRFdata contains Mappability exclusion regions generated
#' using NxtIRF, suitable for use in generating references based on hg38,
#' hg19, mm10 and mm9 genomes.
#' @param genome_type Either one of `hg38`, `hg19`, `mm10` or `mm9`
#' @param path (Default = ".") The desired destination path in which to place a 
#'   copy of the
#'   files. The directory does not need to exist but its parent directory does
#' @param overwrite Whether or not to overwrite files if they already exist
#'   in the given path. Default = FALSE
#' @param offline Whether or not to work in offline mode. This may be suitable
#'   if these functions have been previously run and the user wishes to run
#'   these functions without fetching online hub resources. Default = FALSE
#' @return See Examples section below.
#' @examples
#' # returns the location of the genome.fa file of the mock reference
#' mock_genome() 
#'
#' # returns the location of the transcripts.gtf file of the mock reference
#' mock_gtf() 
#'
#' # Fetches data from ExperimentHub and places them in the given path
#' # returns the locations of the 6 example bam files
#' example_bams(path = tempdir()) 
#'
#' # Fetches data from AnnotationHub and places them in the given path
#' # returns the location of the Mappability exclusion BED for hg38
#' get_mappability_exclusion(genome_type = "hg38", path = tempdir()) 
#'  
#' @references
#' Generation of the mappability files was performed using NxtIRF using
#' a method analogous to that described in:
#' 
#' Middleton R, Gao D, Thomas A, Singh B, Au A, Wong JJ, Bomane A, Cosson B, 
#' Eyras E, Rasko JE, Ritchie W.
#' IRFinder: assessing the impact of intron retention on mammalian gene expression.
#' Genome Biol. 2017 Mar 15;18(1):51.
#' \url{https://doi.org/10.1186/s13059-017-1184-4}
#' @name NxtIRFdata-package
#' @aliases 
#' mock_genome
#' mock_gtf
#' example_bams
#' get_mappability_exclusion
#' @keywords package
#' @md
NULL

#' @describeIn NxtIRFdata-package Returns the location of the genome.fa file of 
#' the mock reference
#' @export
mock_genome <- function()
{
    system.file("extdata", "genome.fa", 
        package="NxtIRFdata", mustWork=TRUE)
}

#' @describeIn NxtIRFdata-package Returns the location of the transcripts.gtf 
#' file of the mock reference
#' @export
mock_gtf <- function()
{
    system.file("extdata", "transcripts.gtf", 
        package="NxtIRFdata", mustWork=TRUE)
}

#' @describeIn NxtIRFdata-package Fetches data from ExperimentHub and places 
#' them in the given path; returns the locations of the 6 example bam files
#' @export
example_bams <- function(path = ".", overwrite = FALSE, offline = FALSE)
{
    if(!file.exists(dirname(path)))
        stop("Cannot create directory for given path")
    if(!file.exists(path)) dir.create(path)

    bam_samples <- c("02H003", "02H025", "02H026", "02H033", "02H043", "02H046")
    files_to_make = sprintf(file.path(path, "%s.bam"), bam_samples)

    if(all(file.exists(files_to_make)) & !overwrite) return(files_to_make)

    hubobj = ExperimentHub::ExperimentHub(localHub = offline)
    titles = sprintf("NxtIRF/example_bam/%s", bam_samples)
    files = c()
    for(i in seq_len(length(titles))) {
        title = titles[i]
        file_to_make = files_to_make[i]
        if(!file.exists(file_to_make) | overwrite) {
            files = append(files, 
                hub_to_file(title, file_to_make, overwrite, hubobj))        
        } else {
            files = append(files, file_to_make)
        }
    }
    if(length(files) <= length(bam_samples)) {
        message("Some BAM files could not be found on ExperimentHub")
    }
    return(files)
}

#' @describeIn NxtIRFdata-package Fetches data from AnnotationHub and places 
#' a copy in the given path; returns the location of this Mappability exclusion 
#' BED file
#' @export
get_mappability_exclusion <- function(
        genome_type = c("hg38", "hg19", "mm10", "mm9"),
    path = ".", overwrite = FALSE, offline = FALSE
) {
    genome_type = match.arg(genome_type)
    if(genome_type == "") return("")

    if(!file.exists(dirname(path)))
        stop("Cannot create directory for given path")
    if(!file.exists(path)) dir.create(path)

    # Check final destination path exists and quickly return if not overwrite
    destfile = file.path(path, paste(genome_type,
        "MappabilityExclusion.bed", sep = "."))
    if(file.exists(destfile) & !overwrite) return(destfile)

    title = paste("NxtIRF", "mappability", genome_type, sep="/")
    hubobj = AnnotationHub::AnnotationHub(localHub = offline)
    record_name = names(hubobj[hubobj$title == title])
    if(length(record_name) < 1) {
        stopmsg = paste("Mappability record not found - ", genome_type)
        message(stopmsg)
        return("")
    } else if(length(record_name) > 1) {
        stopmsg = paste("Multiple mappability records found - ", genome_type)
        message(stopmsg)
        return("")
    }
    gr = hubobj[[record_name]]  # GRanges object from Rds
    
    rtracklayer::export(gr, destfile, "bed")
    file.remove(file)
    return(destfile)
}

# Internal use only
hub_to_file <- function(title, destfile, overwrite = FALSE, hubobj) {
    cache_loc = ""
    if(exists(destfile) & !overwrite) {
        return(destfile)
    }    
    record = hubobj[grepl(title, hubobj$title)]
    if(length(record) > 1) {
        stopmsg = paste("Multiple hub records exist -", title,
            "- please inform developer of this bug")
        message(stopmsg)
        return("")
    } else if(length(record) == 0) {
        stopmsg = paste("Hub record not found -", title)
        message(stopmsg)
        return("")
    }
    fetch_msg = paste("Downloading record from hub, as required:", title)
    message(fetch_msg)
    cache_loc = AnnotationHub::cache(record)[1]     # BAM is the first file
    if(file.exists(cache_loc)) {
        if(file.exists(destfile)) file.remove(destfile)
        file.copy(cache_loc, destfile)
        return(destfile)
    } else {
        stopmsg = paste("Cache fetching failed -", title)
        message(stopmsg)
        return("")
    }
}