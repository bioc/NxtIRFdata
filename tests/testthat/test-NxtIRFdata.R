test_that("chrZ genome exists", {
    expect_equal(file.exists(chrZ_genome()), TRUE)
})

test_that("chrZ gene annotations exists", {
    expect_equal(file.exists(chrZ_gtf()), TRUE)
})

test_that("Resources are available on ExperimentHub", {
    require(ExperimentHub)
    eh = ExperimentHub()
    records = query(eh, "NxtIRF")
    expect_equal(length(records), 10)
    cache_loc = list()
    for(i in seq_len(length(records))) {
        record = records[i]
        cache_loc[[names(record)]] <- ""
        tryCatch({
            cache_loc[[names(record)]] <- cache(record)
        }, error = function(e) {
            cache_loc[[names(record)]] <- ""
        })
    }
    expect_equal(all(file.exists(cache_loc[["EH6783"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6784"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6785"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6786"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6787"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6788"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6789"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6790"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6791"]])), TRUE)
    expect_equal(all(file.exists(cache_loc[["EH6792"]])), TRUE)
})

test_that("example BAMs exists", {
    bams = example_bams()
    expect_equal(all(file.exists(bams)), TRUE)
})

test_that("MappabilityExclusion BED references exist", {
    genome_type = c("hg38", "hg19", "mm10", "mm9")
    files = c()
    for(genome in genome_type) {        
        expect_equal(file.exists(
            get_mappability_exclusion(
                genome_type = genome, as_type = "bed.gz", path = tempdir()
            )
        ), TRUE)
    }
})
