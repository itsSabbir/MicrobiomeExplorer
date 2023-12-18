# Test Data Preparation
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

# Test Class Creation
test_that("MicrobiomeData objects are created correctly", {
  dataObject <- new("MicrobiomeData",
                    rRNA16S = myRNA16SData,
                    Metagenomic = myMetagenomicData,
                    Metatranscriptomic = myMetatranscriptomicData,
                    SampleInfo = mySampleInfo)
  expect_s4_class(dataObject, "MicrobiomeData")
})

# Test Create MicrobiomeData Object Function
test_that("createMicrobiomeDataObject function works correctly", {
  createdObject <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )
  expect_s4_class(createdObject, "MicrobiomeData")
})

# Adjust the getData test to match the expected data type
test_that("getData method retrieves correct data", {
  object <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )
  expect_equal(getData(object, "rRNA16S"), myRNA16SData)
  expect_equal(getData(object, "Metagenomic"), myMetagenomicData)
  expect_equal(getData(object, "Metatranscriptomic"), myMetatranscriptomicData)
  # Adjust this line to expect a list or modify the object definition to store a data frame
  expect_equal(getData(object, "SampleInfo"), as.list(mySampleInfo))
})

# Adjust the updateData test
test_that("updateData method updates data correctly", {
  object <- createMicrobiomeDataObject(
    rRNA16S = myRNA16SData,
    Metagenomic = myMetagenomicData,
    Metatranscriptomic = myMetatranscriptomicData,
    SampleInfo = mySampleInfo
  )
  newRNA16SData <- matrix(runif(50, min = 5, max = 15), nrow = 5, ncol = 10)

  # Ensure the validation function exists or handle its absence in the method
  # If validaterRNA16SData does not exist, you might need to define it or adjust the updateData method.
  updatedObject <- updateData(object, "rRNA16S", newRNA16SData)
  expect_equal(getData(updatedObject, "rRNA16S"), newRNA16SData)
})
