

# Mock Data Preparation
myRNA16SData <- matrix(runif(50, min = 0, max = 10), nrow = 5, ncol = 10)
colnames(myRNA16SData) <- paste0("OTU", 1:10)
rownames(myRNA16SData) <- paste0("Sample", 1:5)

myMetagenomicData <- matrix(rnorm(20, mean = 10), nrow = 5, ncol = 4)
colnames(myMetagenomicData) <- paste0("Gene", 1:4)

myMetatranscriptomicData <- matrix(rnorm(20, mean = 5), nrow = 5, ncol = 4)
colnames(myMetatranscriptomicData) <- paste0("Expression", 1:4)

mySampleInfo <- data.frame(
  SampleID = paste0("Sample", 1:5),
  Condition = rep(c("Control", "Treatment"), length.out = 5)
)
rownames(mySampleInfo) <- mySampleInfo$SampleID

# Test Add Data Function
test_that("addData function works correctly", {
  baseObject <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )

  new16SData <- matrix(runif(40), nrow = 4, ncol = 10)
  rownames(new16SData) <- paste0("NewSample", 1:4)
  colnames(new16SData) <- colnames(myRNA16SData)  # Align column names

  updatedObject <- addData(baseObject, new16SData, "rRNA16S")
  expect_s4_class(updatedObject, "MicrobiomeData")

  addedData <- getData(updatedObject, "rRNA16S")
  expect_true(all(rownames(new16SData) %in% rownames(addedData)))
})



# Test Remove Data Function
test_that("removeData function works correctly", {
  baseObject <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )

  objectWithoutRNA <- removeData(baseObject, "rRNA16S")
  expect_s4_class(objectWithoutRNA, "MicrobiomeData")

  # Further checks to ensure rRNA16S data is removed
  expect_null(slot(objectWithoutRNA, "rRNA16S"))

})

# Test edge cases for removeData
test_that("removeData handles errors correctly", {
  baseObject <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )

  # Test with an invalid data type
  expect_error(removeData(baseObject, "InvalidType"), "Invalid data type")

  # Test with an object that is not of class MicrobiomeData
  expect_error(removeData(list(), "rRNA16S"), "Object must be of class 'MicrobiomeData'")
})


# Test Update Sample Information Function
test_that("updateSampleInfo function works correctly", {
  baseObject <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )

  newSampleInfo <- data.frame(
    SampleID = paste0("Sample", 1:5),
    NewInfo = rep(c("Info1", "Info2"), length.out = 5)
  )
  rownames(newSampleInfo) <- newSampleInfo$SampleID

  updatedObject <- updateSampleInfo(baseObject, newSampleInfo)
  expect_s4_class(updatedObject, "MicrobiomeData")

  updatedSampleInfo <- getData(updatedObject, "SampleInfo")
  # Convert to list for comparison if necessary
  newSampleInfoList <- as.list(newSampleInfo)
  expect_equal(updatedSampleInfo, newSampleInfoList)

})

# Tests for mergeData helper function
test_that("mergeData function works correctly", {
  # Set up two datasets with some overlapping and some unique samples/features
  existingData <- matrix(runif(20), nrow = 4, ncol = 5)
  newData <- matrix(runif(20), nrow = 4, ncol = 5)

  mergedData <- mergeData(existingData, newData, merge_strategy = "average")

  expect_equal(dim(mergedData), c(4, 5)) # Modify as necessary based on expected merging result

})

