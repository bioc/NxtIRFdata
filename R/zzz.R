#' NxtIRFdata
#'
#' This package contains a workable example for the NxtIRF package.
#' It is based on an artificial chromosome Z,
#' as well as 6 example bam files based on samples from the Leucegene dataset
#' (GSE67039). The genes SRSF1, SRSF2, SRSF3, TRA2A, TRA2B, TP53 and NSUN5
#' sequences are used to construct chromosome Z, and the GTF denotes the
#' coordinates of these, based on Ensembl release 94. Bam files are constructed
#' based on the complete bam files of 6 samples from Leucegene. Bam files
#' are subset by regions of the 7 above genes, then the reads of these were
#' realigned to the mock reference using STAR.\cr\cr
#' Additionally, NxtIRFdata contains Mappability exclusion regions generated
#' using NxtIRF, suitable for use in generating referenced based on hg38,
#' hg19, mm10 and mm9 genomes.
#' @return See Examples section below.
#' @examples
#' mock_genome() # returns the genome.fa file of the mock reference
#'
#' mock_gtf() # returns the transcripts.gtf file of the mock reference
#'
#' example_bams() # returns the locations of the 6 example bam files
#'
#' get_mappability_exclusion("hg38") # Mappability exclusion BED for hg38
#' @docType package
#' @name NxtIRFdata-package
#' @aliases NxtIRFdata-package mock_genome() mock_gtf() example_bams()
#' @aliases get_mappability_exclusion()
#' @keywords package
#' @md
NULL

#' @describeIn NxtIRFdata-package Returns the location of the mock genome.fa file
#' @export
mock_genome <- function()
{
    system.file("extdata", "genome.fa",
        package="NxtIRFdata", mustWork=TRUE)
}

#' @describeIn NxtIRFdata-package Returns the location of the mock gene annotations transcripts.gtf
#' @export
mock_gtf <- function()
{
    system.file("extdata", "transcripts.gtf",
        package="NxtIRFdata", mustWork=TRUE)
}

#' @describeIn NxtIRFdata-package Returns the location of the example BAM files for NxtIRF, aligned to the mock genome
#' @export
example_bams <- function()
{
    c(
        system.file("extdata", "02H003_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE),
        system.file("extdata", "02H025_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE),    
        system.file("extdata", "02H026_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE),    
        system.file("extdata", "02H033_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE),    
        system.file("extdata", "02H043_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE),    
        system.file("extdata", "02H046_chrZ.bam",
            package="NxtIRFdata", mustWork=TRUE)  
    )
}

#' @describeIn NxtIRFdata-package Returns the location of the Mappability Exclusion BED files for NxtIRF
#' @param genome_type Either one of `hg38`, `hg19`, `mm10` or `mm9`
#' @export
get_mappability_exclusion <- function(
        genome_type = c("hg38", "hg19", "mm10", "mm9")) {
    genome_type = match.arg(genome_type)
    if(genome_type == "hg38") {
        system.file("extdata", "Mappability_Regions_hg38_v94.txt.gz",
            package="NxtIRFdata", mustWork=TRUE)    
    } else if(genome_type == "hg19") {
        system.file("extdata", "Mappability_Regions_hg19_v75.txt.gz",
            package="NxtIRFdata", mustWork=TRUE)        
    } else if(genome_type == "mm10") {
        system.file("extdata", "Mappability_Regions_mm10_v94.txt.gz",
            package="NxtIRFdata", mustWork=TRUE)        
    } else if(genome_type == "mm9") {
        system.file("extdata", "Mappability_Regions_mm9_v67.txt.gz",
            package="NxtIRFdata", mustWork=TRUE)        
    } else {
        stop(paste("In get_mappability_exclusion():",
            "genome_type = ", genome_type, "is not recogised"
        ), call. = FALSE)
    }
}