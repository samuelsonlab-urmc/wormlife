###############################################################################
# ---Template script for statistical analysis of replica set data
#     with WormLife---
# Samuelson Lab, URMC 2017
# https://github.com/samuelsonlab-urmc/wormlife
#
# Use this script as the basis for running your own analyses by modifying the
#  relevant arguments/parameters for your dataset.
# "Out of the box" this is set up for analysis of the example replica set
#  file, provided with the program.
#  
# A pre-formatted input data file is necessary (see example data file for format).
# An addiional file listing the comparisons to to run (out of the set of all possible combinations
#  of strain and treatment) can be specified, optional but suggested for studies with many treatments
#  where not all cases need to be compared.
#
# 1) Edit parameters in the "SET ARGUMENTS AND PARAMETERS" section
# 2) Run the whole file: the "source" button in RStudio; edit menu -> "source document" in the R GUI
# 3) A CSV file with the results should be written when everything has worked right.
#     When things don't work right check your input data format. In many cases there should be an
#     error message that is informative.
#
###############################################################################

# ----------  SET ARGUMENTS AND PARAMETERS ----------

## Set random seed for consistency between runs
### arbitrary number- keep consistent for reproducible results between runs
### with the same data and number of iterations of resampling
set.seed(0123456)

## Set location of WormLife Plottool folder
### Note: R always uses "/" to delineate folders, even on Windows.
wlDir <- "/SET_FOLDER_LOCATION/WormLife_0.4"

## Set location of desired data file
### This is the APPROPRIATELY FORMATTED AND FILTERED file with 
### the data to run statistics on
### The default here is the example file.
dataFile <- file.path(wlDir, "example_data", "Table S1- Example replica set survival data.csv")

## Specify columns in data file
### Specify which columns in the data file correspond to which of the required columns
### These are, by default, set to the column names for the example data file
col.strain <- "strain_genotype"
col.treat <- "rnai_gene_name"
col.trial <- "trial_num"
col.time <- "day_adult"
col.dead <- "dead_num"
col.total <- "total_num"

## Comparison file
### Specify the location of the comparison file- this file lists the comparisons
### between sample groups that are to be run
### This is useful if you have many sample groups, but only want to compare a subset of them.
### See the example file for the format. All group names must be in the form "strain_treatment"
### where both strain and treatment are from the input data file. You will be informed if names do not match.
### If set to NA then all possible comparisons will be run
#compFile <- NA
compFile <- file.path(wlDir, "example_data", "comps_rsm_example.csv")

## Output directory
### Set the path of the folder to write output files to
outputPath <- file.path(wlDir, "example_output")
# create this folder if it does not exist
if(!dir.exists(outputPath)){dir.create(outputPath)}

## Output file base name
### The "prefix" to use for output file names
outputFileName <- "RSM_example"

## Number of iterations for resampling per comparison
### Higher number = more precise p-values, slower run time
### Lower number = less precise p-values, faster run time
### P-values of 0 MAY be reported if number of iterations is too low
### In such cases, it is appropriate to report "p < 0.01" if k.resamp = 100,
### or "p < 0.001" if k.resamp = 1000 etc
k.resamp <- 1000

## Number of CPU cores to use
# If set to NA (default), will use all available cores
# Set to a different number if desired
nCores <- NA

## Todo: option to subset by trial, or produce results for individual trials

# ----------  LOAD NECESSARY PACKAGES ----------
# install openxlsx and then uncomment the following line for loading Excel files
# library(openxlsx)

#=======================================================================================
# ----------------- BELOW HERE WONT NEED TO BE EDITED MOST OF THE TIME -----------------
#=======================================================================================

# ----------  READ IN CODE FILES ----------
invisible(sapply(file.path(wlDir, "code",
                 c("ld50.R", "logit.R", "maxday.R", "models.R",
                   "tabline.R", "wormCompare.R", "test_logit_parallel.R",
                   "generic_tools.R", "modelData.R", "plotting.R", "points.R")), source))

# ----------  READ IN AND CHECK DATA FILES ----------
data_table <- read.csv(dataFile, header = TRUE, row.names = NULL, stringsAsFactors = FALSE)

# subset to necessary columns and change to necessary names
# column names MUST include "time", "dead", "total" ! (can't use alternative name for time col)
data_table.2 <- data.frame(data_table[ , c(col.strain, col.treat, col.time, col.dead, col.total) ],
                           stringsAsFactors = FALSE)
colnames(data_table.2) <- c("Strain", "rnai", "time", "dead", "total")

# check data types for values that should be numeric
if(!(class(data_table.2$time) %in% c("integer", "numeric"))){
  stop(paste("Data quality issue: Time column has some non-numeric values"))
}else if(!(class(data_table.2$time) %in% c("integer", "numeric"))){
  stop(paste("Data quality issue: dead column has some non-numeric values"))
}else if(!(class(data_table.2$dead) %in% c("integer", "numeric"))){
  stop(paste("Data quality issue: total column has some non-numeric values"))
}

# check for NAs- by default these will be removed
# will error if more than 10% of all rows have a column with an NA value
if(sum(rowSums(is.na(data_table.2)) > 0) > nrow(data_table.2) * 0.1){
  stop("Data quality issue: more than 10% of all rows in file have NA values in at least one column")
}else if(any(rowSums(is.na(data_table.2)) > 0)){
  print(paste("Found", sum(rowSums(is.na(data_table.2)) > 0), "rows with NA values, removing..."))
  data_table.2 <- data_table.2[ -which(rowSums(is.na(data_table.2)) > 0) , ]
}

# any cases where there are more dead than total?
if(any(data_table.2$dead > data_table.2$total)){
  stop(paste("Data quality issue: found more dead than total on input file lines",
             which(data_table.2$dead > data_table.2$total)))
}

# ----------  RUN! ----------

# group full table into groups by condition (in this case strain + rnai)
data.list <- getTablist(data_table.2, IDcols = c("Strain", "rnai"))

# remove any conditions that have fewer than five rows (observations)
if(any(unlist(lapply(data.list, FUN = function(x){nrow(x)})) < 5)){
  print("Some strain/treatment combinations with fewer than five observations- removing")
  data.list <- data.list[-which(unlist(lapply(data.list, FUN = function(x){nrow(x)})) < 5)]
}
# check to make sure there at at least two strain/treatment levels left
if(length(data.list) < 2){
  stop("Not enough strain/treatment combinations for comparisons")
}

# This sets up models per-condition (two-parameter logit survival GLM)
# note: columns has to be NAMED vector
data.models <- lapply(seq_along(data.list),
                      FUN = function(i){wormlogit(data.list[[i]], ID = names(data.list)[i],
                                                  columns = setNames(c("time", "dead", "total"),
                                                                     c("time", "dead", "total")))})
names(data.models) <- unlist(lapply(data.models, FUN = function(x){x$ID}))

# if applicable, load and check the comparison file
# if NA, then use all possible comparisons
if(is.na(compFile)){
  # generate all possible combinations between experimental and control conditions
  data.testing <- expand.grid(names(data.models), names(data.models), stringsAsFactors = FALSE)
  data.testing$Var1 <- as.character(data.testing$Var1)
  data.testing$Var2 <- as.character(data.testing$Var2)
  colnames(data.testing) <- c("cond1", "cond2")
}else{
  data.testing <- read.csv(compFile, header = TRUE, stringsAsFactors = FALSE)
  if(!(ncol(data.testing) == 2)){stop("Error: comparison file not expected format")}
  if((nrow(data.testing) < 1)){stop("Error: comparison file has no rows")}
  if(any(!(unique(data.testing[,1]) %in% names(data.models)))){
    stop("Error: Comparison file class in column 1 not represented in dataset")
  }
  if(any(!(unique(data.testing[,2]) %in% names(data.models)))){
    stop("Error: Comparison file class in column 2 not represented in dataset")
  }
  data.testing <- data.frame(data.testing, stringsAsFactors = FALSE)
}

data.testing$result[1:nrow(data.testing)] <- list(list())
data.testing$result.pval <- rep(NA, nrow(data.testing))

# set up parallel workers
cl <- makeForkCluster(nnodes = ifelse(is.na(nCores), getOption("mc.cores", 2L), nCores))
registerDoParallel(cl)
getDoParWorkers()

#
### ----- Most of the work happens in this loop -----
#
for(i in (1:nrow(data.testing))){
  data.testing$result[[i]] <- test_logit(g1 = data.models[[as.character(data.testing$cond1[i])]],
                                         g2 = data.models[[as.character(data.testing$cond2[i])]],
                                         k = k.resamp, quiet = TRUE, p.only = FALSE, prop = FALSE)
  data.testing$result.pval[i] <- data.testing$result[[i]]$p
}

stopCluster(cl)

test_res <- data.frame(data.testing[ , c("cond1", "cond2", "result.pval")],
                                 cond1_median = unlist(lapply(data.testing$result, FUN = function(x){x$condition["ld1"]})),
                                 cond2_median = unlist(lapply(data.testing$result, FUN = function(x){x$condition["ld2"]})),
                                 stringsAsFactors = FALSE)

# correct p-values to account for multiple testing
test_res$adj_pval <- p.adjust(test_res$result.pval, method = "fdr")


# ----------  OUTPUT RESULTS ----------

# calculate pct changed between conditions based on median survival
test_res$pct_chng <- ((test_res$cond1_median - test_res$cond2_median)/test_res$cond2_median) * 100

write.csv(x = test_res, file.path(outputPath, paste0(outputFileName, " survival comparison test results.csv")),
          row.names = FALSE)


# ----------  THATS IT FOT NOW...  ----------
