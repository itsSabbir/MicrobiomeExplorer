# Mock Microbiome Dataframe Creation
set.seed(123)  # For reproducibility
sample_size <- 10  # Number of samples
taxa_number <- 5   # Number of microbial taxa

# Create a dataframe with random counts
microbiome_example <- as.data.frame(matrix(rnorm(sample_size * taxa_number, mean = 10, sd = 5),
                                        nrow = sample_size,
                                        ncol = taxa_number))
colnames(microbiome_example) <- paste("Taxa", 1:taxa_number, sep = "_")
rownames(microbiome_example) <- paste("Sample", 1:sample_size, sep = "_")
