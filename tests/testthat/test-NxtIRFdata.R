test_that("chrZ genome exists", {
    expect_equal(
        file.exists(chrZ_genome()), 
        TRUE
    )
})

test_that("chrZ gene annotations exists", {
    expect_equal(
        file.exists(chrZ_gtf()), 
        TRUE
    )
})

test_that("example BAMs exists", {
    expect_equal(
        all(file.exists(example_bams(path = tempdir()))), 
        TRUE
    )
})

test_that("MappabilityExclusion BED references exist", {
    genome_type = c("hg38", "hg19", "mm10", "mm9")
    for(genome in genome_type) {
        expect_equal(
            file.exists(
                get_mappability_exclusion(
                    genome_type = genome,
                    path = tempdir()
                )
            ), 
            TRUE
        )
    }
})
