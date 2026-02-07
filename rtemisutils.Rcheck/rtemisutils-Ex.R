pkgname <- "rtemisutils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rtemisutils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_dependencies")
### * check_dependencies

flush(stderr()); flush(stdout())

### Name: check_dependencies
### Title: 'rtemis' internal: Dependencies check
### Aliases: check_dependencies

### ** Examples

# This will throw an error if "unavailable" is not installed:
# check_dependencies("unavailable")



cleanEx()
nameEx("clean_colnames")
### * clean_colnames

flush(stderr()); flush(stdout())

### Name: clean_colnames
### Title: Clean column names
### Aliases: clean_colnames

### ** Examples

clean_colnames(iris)



cleanEx()
nameEx("clean_int")
### * clean_int

flush(stderr()); flush(stdout())

### Name: clean_int
### Title: Clean integer input
### Aliases: clean_int

### ** Examples

## Not run: 
##D clean_int(6L)
##D clean_int(3)
##D clean_int(12.1) # Error
##D clean_int(c(3, 5, 7))
##D clean_int(c(3, 5, 7.01)) # Error
## End(Not run)



cleanEx()
nameEx("clean_names")
### * clean_names

flush(stderr()); flush(stdout())

### Name: clean_names
### Title: Clean names
### Aliases: clean_names

### ** Examples

x <- c("Patient ID", "_Date-of-Birth", "SBP (mmHg)")
x
clean_names(x)



cleanEx()
nameEx("col2grayscale")
### * col2grayscale

flush(stderr()); flush(stdout())

### Name: col2grayscale
### Title: Color to Grayscale
### Aliases: col2grayscale

### ** Examples

## Not run: 
##D col2grayscale("red")
##D col2grayscale("red", "dec")
## End(Not run)



cleanEx()
nameEx("col2hex")
### * col2hex

flush(stderr()); flush(stdout())

### Name: col2hex
### Title: Convert R color to hexadecimal code
### Aliases: col2hex

### ** Examples

## Not run: 
##D col2hex(c("gray50", "skyblue"))
## End(Not run)



cleanEx()
nameEx("color_invertRGB")
### * color_invertRGB

flush(stderr()); flush(stdout())

### Name: color_invertRGB
### Title: Invert Color in RGB space
### Aliases: color_invertRGB

### ** Examples

## Not run: 
##D cols <- c("red", "green", "blue")
##D previewcolor(cols)
##D cols |>
##D   color_invertRGB() |>
##D   previewcolor()
## End(Not run)



cleanEx()
nameEx("color_mix")
### * color_mix

flush(stderr()); flush(stdout())

### Name: color_mix
### Title: Create an alternating sequence of graded colors
### Aliases: color_mix

### ** Examples

## Not run: 
##D color <- list(
##D   blue = c("#82afd3", "#000f3a"),
##D   gray = c("gray10", "gray85")
##D )
##D previewcolor(desaturate(color_mix(color, 6), .3))
##D 
##D color <- list(
##D   blue = c("#82afd3", "#57000a"),
##D   gray = c("gray10", "gray85")
##D )
##D previewcolor(desaturate(color_mix(color, 6), .3))
##D 
##D color <- list(
##D   blue = c("#82afd3", "#000f3a"),
##D   purple = c("#23001f", "#c480c1")
##D )
##D previewcolor(desaturate(color_mix(color, 5), .3))
## End(Not run)



cleanEx()
nameEx("ddSci")
### * ddSci

flush(stderr()); flush(stdout())

### Name: ddSci
### Title: Format Numbers for Printing
### Aliases: ddSci

### ** Examples

x <- .34876549
ddSci(x)
# "0.35"
x <- .00000000457823
ddSci(x)
# "4.6e-09"



cleanEx()
nameEx("desaturate")
### * desaturate

flush(stderr()); flush(stdout())

### Name: desaturate
### Title: Pastelify a color (make a color more pastel)
### Aliases: desaturate

### ** Examples

## Not run: 
##D cols <- c("red", "green", "blue")
##D previewcolor(cols)
##D cols_d <- desaturate(cols)
##D previewcolor(cols_d)
## End(Not run)



cleanEx()
nameEx("df_movecolumn")
### * df_movecolumn

flush(stderr()); flush(stdout())

### Name: df_movecolumn
### Title: Move data frame column
### Aliases: df_movecolumn

### ** Examples

## Not run: 
##D ir <- df_movecolumn(iris, colname = "Species", to = 1L)
## End(Not run)



cleanEx()
nameEx("drange")
### * drange

flush(stderr()); flush(stdout())

### Name: drange
### Title: Set Dynamic Range
### Aliases: drange

### ** Examples

## Not run: 
##D x <- runif(20, -10, 10)
##D x <- drange(x)
## End(Not run)



cleanEx()
nameEx("dt_describe")
### * dt_describe

flush(stderr()); flush(stdout())

### Name: dt_describe
### Title: Describe data.table
### Aliases: dt_describe

### ** Examples

library(data.table)
origin <- as.POSIXct("2022-01-01 00:00:00", tz = "America/Los_Angeles")
x <- data.table(
  ID = paste0("ID", 1:10),
  V1 = rnorm(10),
  V2 = rnorm(10, 20, 3),
  V1_datetime = as.POSIXct(
    seq(
      1, 1e7,
      length.out = 10
    ),
    origin = origin
  ),
  V2_datetime = as.POSIXct(
    seq(
      1, 1e7,
      length.out = 10
    ),
    origin = origin
  ),
  C1 = sample(c("alpha", "beta", "gamma"), 10, TRUE),
  F1 = factor(sample(c("delta", "epsilon", "zeta"), 10, TRUE))
)



cleanEx()
nameEx("dt_inspect_types")
### * dt_inspect_types

flush(stderr()); flush(stdout())

### Name: dt_inspect_types
### Title: Inspect column types
### Aliases: dt_inspect_types

### ** Examples

library(data.table)
x <- data.table(
  id = 8001:8006,
  a = c("3", "5", "undefined", "21", "4", NA),
  b = c("mango", "banana", "tangerine", NA, "apple", "kiwi"),
  c = c(1, 2, 3, 4, 5, 6)
)
dt_inspect_types(x)



cleanEx()
nameEx("dt_keybin_reshape")
### * dt_keybin_reshape

flush(stderr()); flush(stdout())

### Name: dt_keybin_reshape
### Title: Long to wide key-value reshaping
### Aliases: dt_keybin_reshape

### ** Examples

library(data.table)
x <- data.table(
  ID = rep(1:3, each = 2),
  Dx = c("A", "C", "B", "C", "D", "A")
)
dt_keybin_reshape(x, id_name = "ID", key_name = "Dx")



cleanEx()
nameEx("dt_merge")
### * dt_merge

flush(stderr()); flush(stdout())

### Name: dt_merge
### Title: Merge data.tables
### Aliases: dt_merge

### ** Examples

library(data.table)
xleft <- data.table(ID = 1:5, Alpha = letters[1:5])
xright <- data.table(ID = c(3, 4, 5, 6), Beta = LETTERS[3:6])
xlr_inner <- dt_merge(xleft, xright, on = "ID", how = "inner")



cleanEx()
nameEx("dt_nunique_perfeat")
### * dt_nunique_perfeat

flush(stderr()); flush(stdout())

### Name: dt_nunique_perfeat
### Title: Number of unique values per feature
### Aliases: dt_nunique_perfeat

### ** Examples

library(data.table)
ir <- as.data.table(iris)
dt_nunique_perfeat(ir)



cleanEx()
nameEx("dt_pctmatch")
### * dt_pctmatch

flush(stderr()); flush(stdout())

### Name: dt_pctmatch
### Title: Get N and percent match of values between two columns of two
###   data.tables
### Aliases: dt_pctmatch

### ** Examples

library(data.table)
x <- data.table(ID = 1:5, Alpha = letters[1:5])
y <- data.table(ID = c(3, 4, 5, 6), Beta = LETTERS[3:6])
dt_pctmatch(x, y, on = "ID")



cleanEx()
nameEx("dt_pctmissing")
### * dt_pctmissing

flush(stderr()); flush(stdout())

### Name: dt_pctmissing
### Title: Get percent of missing values from every column
### Aliases: dt_pctmissing

### ** Examples

library(data.table)
x <- data.table(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4), c = c("A", "B", "C", NA))
dt_pctmissing(x)



cleanEx()
nameEx("dt_set_autotypes")
### * dt_set_autotypes

flush(stderr()); flush(stdout())

### Name: dt_set_autotypes
### Title: Set column types automatically
### Aliases: dt_set_autotypes

### ** Examples

library(data.table)
x <- data.table(
  id = 8001:8006,
  a = c("3", "5", "undefined", "21", "4", NA),
  b = c("mango", "banana", "tangerine", NA, "apple", "kiwi"),
  c = c(1, 2, 3, 4, 5, 6)
)
str(x)
# ***in-place*** operation means no assignment is needed
dt_set_autotypes(x)
str(x)

# Try excluding column 'a' from autotyping
x <- data.table(
  id = 8001:8006,
  a = c("3", "5", "undefined", "21", "4", NA),
  b = c("mango", "banana", "tangerine", NA, "apple", "kiwi"),
  c = c(1, 2, 3, 4, 5, 6)
)
str(x)
# exclude column 'a' from autotyping
dt_set_autotypes(x, cols = setdiff(names(x), "a"))
str(x)



cleanEx()
nameEx("dt_set_clean_all")
### * dt_set_clean_all

flush(stderr()); flush(stdout())

### Name: dt_set_clean_all
### Title: Clean column names and factor levels _*in-place*_
### Aliases: dt_set_clean_all

### ** Examples

library(data.table)
x <- as.data.table(iris)
levels(x[["Species"]]) <- c("setosa:iris", "versicolor$iris", "virginica iris")
names(x)
levels(x[["Species"]])
# ***in-place*** operation means no assignment is needed
dt_set_clean_all(x)
names(x)
levels(x[["Species"]])



cleanEx()
nameEx("dt_set_cleanfactorlevels")
### * dt_set_cleanfactorlevels

flush(stderr()); flush(stdout())

### Name: dt_set_cleanfactorlevels
### Title: Clean factor levels of data.table _*in-place*_
### Aliases: dt_set_cleanfactorlevels

### ** Examples

library(data.table)
x <- as.data.table(iris)
levels(x[["Species"]]) <- c("setosa:iris", "versicolor$iris", "virginica iris")
levels(x[["Species"]])
dt_set_cleanfactorlevels(x)
levels(x[["Species"]])



cleanEx()
nameEx("dt_set_logical2factor")
### * dt_set_logical2factor

flush(stderr()); flush(stdout())

### Name: dt_set_logical2factor
### Title: Convert data.table logical columns to factors
### Aliases: dt_set_logical2factor

### ** Examples

library(data.table)
x <- data.table(a = 1:5, b = c(TRUE, FALSE, FALSE, FALSE, TRUE))
x
dt_set_logical2factor(x)
x
z <- data.table(
  alpha = 1:5,
  beta = c(TRUE, FALSE, TRUE, NA, TRUE),
  gamma = c(FALSE, FALSE, TRUE, FALSE, NA)
)
# You can usee fillNA to fill NA values with a constant
dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"), fillNA = "No")
z
w <- data.table(mango = 1:5, banana = c(FALSE, FALSE, TRUE, TRUE, FALSE))
w
dt_set_logical2factor(w, cols = 2, labels = c("Ugh", "Huh"))
w
# Column attributes are maintained by default:
z <- data.table(
  alpha = 1:5,
  beta = c(TRUE, FALSE, TRUE, NA, TRUE),
  gamma = c(FALSE, FALSE, TRUE, FALSE, NA)
)
for (i in seq_along(z)) setattr(z[[i]], "source", "Guava")
str(z)
dt_set_logical2factor(z, cols = "beta", labels = c("No", "Yes"))
str(z)



cleanEx()
nameEx("factor_NA2missing")
### * factor_NA2missing

flush(stderr()); flush(stdout())

### Name: factor_NA2missing
### Title: Factor NA to "missing" level
### Aliases: factor_NA2missing

### ** Examples

x <- factor(sample(letters[1:3], 100, TRUE))
x[sample(1:100, 10)] <- NA
xm <- factor_NA2missing(x)



cleanEx()
nameEx("fct_describe")
### * fct_describe

flush(stderr()); flush(stdout())

### Name: fct_describe
### Title: Describe factor
### Aliases: fct_describe

### ** Examples

## Not run: 
##D # Small number of levels
##D fct_describe(iris[["Species"]])
##D 
##D # Large number of levels: show top n by count
##D x <- factor(sample(letters, 1000, TRUE))
##D fct_describe(x)
##D fct_describe(x, 3)
## End(Not run)



cleanEx()
nameEx("filter_order")
### * filter_order

flush(stderr()); flush(stdout())

### Name: filter_order
### Title: Filter order
### Aliases: filter_order

### ** Examples

## Not run: 
##D x <- rnorm(10)
##D x
##D x[filter_order(x, x < 0)]
## End(Not run)



cleanEx()
nameEx("fmt")
### * fmt

flush(stderr()); flush(stdout())

### Name: fmt
### Title: Text formatting
### Aliases: fmt

### ** Examples

# Simple color
fmt("Hello", col = "red")

# Bold red text
fmt("Error", col = "red", bold = TRUE)

# Multiple styles
fmt("Warning", col = "yellow", bold = TRUE, italic = TRUE)

# With background
fmt("Highlight", col = "white", bg = "blue", bold = TRUE)



cleanEx()
nameEx("get_loaded_pkg_version")
### * get_loaded_pkg_version

flush(stderr()); flush(stdout())

### Name: get_loaded_pkg_version
### Title: Get version of all loaded packages (namespaces)
### Aliases: get_loaded_pkg_version

### ** Examples

get_loaded_pkg_version()



cleanEx()
nameEx("get_mode")
### * get_mode

flush(stderr()); flush(stdout())

### Name: get_mode
### Title: Get the mode of a factor or integer
### Aliases: get_mode

### ** Examples

x <- c(9, 3, 4, 4, 0, 2, 2, NA)
get_mode(x)
x <- c(9, 3, 2, 2, 0, 4, 4, NA)
get_mode(x)
get_mode(x, getlast = FALSE)



cleanEx()
nameEx("get_output_type")
### * get_output_type

flush(stderr()); flush(stdout())

### Name: get_output_type
### Title: Get output type
### Aliases: get_output_type

### ** Examples

get_output_type()



cleanEx()
nameEx("getnames")
### * getnames

flush(stderr()); flush(stdout())

### Name: getnames
### Title: Get names by string matching or class
### Aliases: getnames getfactornames getnumericnames getlogicalnames
###   getcharacternames getdatenames

### ** Examples

getnames(iris, starts_with = "Sepal")
getnames(iris, ends_with = "Width")



cleanEx()
nameEx("graph_node_metrics")
### * graph_node_metrics

flush(stderr()); flush(stdout())

### Name: graph_node_metrics
### Title: Node-wise (i.e. vertex-wise) graph metrics
### Aliases: graph_node_metrics

### ** Examples

## Not run: 
##D datcor <- cor(rnormmat(20, 20, seed = 2021))
##D datcor[sample(seq(datcor), 250)] <- 0
##D x <- igraph::graph_from_adjacency_matrix(
##D   adjmatrix = datcor,
##D   mode = "lower",
##D   weighted = TRUE,
##D   diag = FALSE
##D )
##D 
##D graph_node_metrics(x)
## End(Not run)



cleanEx()
nameEx("iflengthy")
### * iflengthy

flush(stderr()); flush(stdout())

### Name: iflengthy
### Title: Return object if it has length > 0
### Aliases: iflengthy

### ** Examples

x <- 2:4
iflengthy(x)
y <- list()
iflengthy(y)



cleanEx()
nameEx("index_col_by_attr")
### * index_col_by_attr

flush(stderr()); flush(stdout())

### Name: index_col_by_attr
### Title: Index columns by attribute name & value
### Aliases: index_col_by_attr

### ** Examples

library(data.table)
x <- data.table(
  id = 1:5,
  sbp = rnorm(5, 120, 15),
  dbp = rnorm(5, 80, 10),
  paO2 = rnorm(5, 90, 10),
  paCO2 = rnorm(5, 40, 5)
)
setattr(x[["sbp"]], "source", "outpatient")
setattr(x[["dbp"]], "source", "outpatient")
setattr(x[["paO2"]], "source", "icu")
setattr(x[["paCO2"]], "source", "icu")
index_col_by_attr(x, "source", "icu")



cleanEx()
nameEx("inspect_type")
### * inspect_type

flush(stderr()); flush(stdout())

### Name: inspect_type
### Title: Inspect character and factor vector
### Aliases: inspect_type

### ** Examples

x <- c("3", "5", "undefined", "21", "4", NA)
inspect_type(x)
z <- c("mango", "banana", "tangerine", NA)
inspect_type(z)



cleanEx()
nameEx("is_constant")
### * is_constant

flush(stderr()); flush(stdout())

### Name: is_constant
### Title: Check if vector is constant
### Aliases: is_constant

### ** Examples

## Not run: 
##D x <- rep(9, 1000000)
##D is_constant(x)
##D x[10] <- NA
##D is_constant(x)
##D is_constant(x, skip_missing = TRUE)
## End(Not run)



cleanEx()
nameEx("labelify")
### * labelify

flush(stderr()); flush(stdout())

### Name: labelify
### Title: Format text for label printing
### Aliases: labelify

### ** Examples

x <- c("county_name", "total.cost$", "age", "weight.kg")
labelify(x)



cleanEx()
nameEx("list2csv")
### * list2csv

flush(stderr()); flush(stdout())

### Name: list2csv
### Title: Write list elements to CSV files
### Aliases: list2csv

### ** Examples

## Not run: 
##D x <- list(
##D    iris = iris,
##D    iris_normalized = as.data.frame(scale(iris[, -5]))
##D )
##D outdir <- "./exports"
##D list2csv(x, outdir)
## End(Not run)



cleanEx()
nameEx("lotri2edgeList")
### * lotri2edgeList

flush(stderr()); flush(stdout())

### Name: lotri2edgeList
### Title: Connectivity Matrix to Edge List
### Aliases: lotri2edgeList

### ** Examples

A <- matrix(rnorm(100), nrow = 10)
A[lower.tri(A)] <- t(A)[lower.tri(A)]
diag(A) <- 1
edgelist <- lotri2edgeList(A, filename = NULL)
head(edgelist)



cleanEx()
nameEx("matchcases")
### * matchcases

flush(stderr()); flush(stdout())

### Name: matchcases
### Title: Match cases by covariates
### Aliases: matchcases

### ** Examples

## Not run: 
##D set.seed(2021)
##D cases <- data.frame(
##D   PID = paste0("PID", seq(4)),
##D   Sex = factor(c(1, 1, 0, 0)),
##D   Handedness = factor(c(1, 1, 0, 1)),
##D   Age = c(21, 27, 39, 24),
##D   Var = c(.7, .8, .9, .6),
##D   Varx = rnorm(4)
##D )
##D controls <- data.frame(
##D   CID = paste0("CID", seq(50)),
##D   Sex = factor(sample(c(0, 1), 50, TRUE)),
##D   Handedness = factor(sample(c(0, 1), 50, TRUE, c(.1, .9))),
##D   Age = sample(16:42, 50, TRUE),
##D   Var = rnorm(50),
##D   Vary = rnorm(50)
##D )
##D 
##D mc <- matchcases(cases, controls, 2, "PID", "CID")
## End(Not run)



cleanEx()
nameEx("mgetnames")
### * mgetnames

flush(stderr()); flush(stdout())

### Name: mgetnames
### Title: Get names by string matching multiple patterns
### Aliases: mgetnames

### ** Examples

mgetnames(iris, pattern = c("Sepal", "Petal"))
mgetnames(iris, starts_with = "Sepal")
mgetnames(iris, ends_with = "Width")



cleanEx()
nameEx("msg")
### * msg

flush(stderr()); flush(stdout())

### Name: msg
### Title: Message with provenance
### Aliases: msg msg0

### ** Examples

msg("Hello, world!")
x <- 42L
msg0("The answer is what you think it is (", x, ").")



cleanEx()
nameEx("names_by_class")
### * names_by_class

flush(stderr()); flush(stdout())

### Name: names_by_class
### Title: List column names by class
### Aliases: names_by_class

### ** Examples

names_by_class(iris)



cleanEx()
nameEx("pcat")
### * pcat

flush(stderr()); flush(stdout())

### Name: pcat
### Title: Pad-cat
### Aliases: pcat

### ** Examples

## Not run: 
##D {
##D   msg("Hello")
##D   pcat("super", "wow")
##D   pcat(NULL, "oooo")
##D }
## End(Not run)



cleanEx()
nameEx("previewcolor")
### * previewcolor

flush(stderr()); flush(stdout())

### Name: previewcolor
### Title: Preview color
### Aliases: previewcolor

### ** Examples

## Not run: 
##D colors <- colorgradient_x(seq(-5, 5))
##D previewcolor(colors)
## End(Not run)



cleanEx()
nameEx("qstat")
### * qstat

flush(stderr()); flush(stdout())

### Name: qstat
### Title: SGE qstat
### Aliases: qstat

### ** Examples

## Not run: 
##D qstat()
## End(Not run)



cleanEx()
nameEx("repr_S7name")
### * repr_S7name

flush(stderr()); flush(stdout())

### Name: repr_S7name
### Title: Show S7 class name
### Aliases: repr_S7name
### Keywords: internal

### ** Examples

repr_S7name("Supervised") |> cat()



cleanEx()
nameEx("rnormmat")
### * rnormmat

flush(stderr()); flush(stdout())

### Name: rnormmat
### Title: Random Normal Matrix
### Aliases: rnormmat

### ** Examples

x <- rnormmat(20, 5, mean = 12, sd = 6, return_df = TRUE, seed = 2026)
x



cleanEx()
nameEx("rt_reactable")
### * rt_reactable

flush(stderr()); flush(stdout())

### Name: rt_reactable
### Title: View table using reactable
### Aliases: rt_reactable

### ** Examples

## Not run: 
##D # needs html viewer
##D rt_reactable(iris, datatypes = sapply(iris, class))
## End(Not run)



cleanEx()
nameEx("rtemis_colors")
### * rtemis_colors

flush(stderr()); flush(stdout())

### Name: rtemis_colors
### Title: rtemis Color System
### Aliases: rtemis_colors
### Keywords: datasets

### ** Examples

rtemis_colors[["rt_teal"]]




cleanEx()
nameEx("rtpalette")
### * rtpalette

flush(stderr()); flush(stdout())

### Name: rtpalette
### Title: Color Palettes
### Aliases: rtpalette

### ** Examples

# Print available palettes
rtpalette()
# Get the Imperial palette
rtpalette("imperial")



cleanEx()
nameEx("rtversion")
### * rtversion

flush(stderr()); flush(stdout())

### Name: rtversion
### Title: Get rtemis version and system info
### Aliases: rtversion

### ** Examples

rtversion()



cleanEx()
nameEx("runifmat")
### * runifmat

flush(stderr()); flush(stdout())

### Name: runifmat
### Title: Random Uniform Matrix
### Aliases: runifmat

### ** Examples

x <- runifmat(20, 5, min = 12, max = 18, return_df = TRUE, seed = 2026)
x



cleanEx()
nameEx("setdiffsym")
### * setdiffsym

flush(stderr()); flush(stdout())

### Name: setdiffsym
### Title: Symmetric Set Difference
### Aliases: setdiffsym

### ** Examples

setdiff(1:10, 1:5)
setdiff(1:5, 1:10)
setdiffsym(1:10, 1:5)
setdiffsym(1:5, 1:10)



cleanEx()
nameEx("sge_submit")
### * sge_submit

flush(stderr()); flush(stdout())

### Name: sge_submit
### Title: Submit expression to SGE grid
### Aliases: sge_submit

### ** Examples

## Not run: 
##D sge_submit({
##D   # Your code here
##D }, obj_names = c("df1", "model1"), packages = c("rtemis", "data.table"),
##D queue = "all.q", n_workers = 4, h_rt = "01:00:00", mem_free = "4G")
## End(Not run)



cleanEx()
nameEx("size")
### * size

flush(stderr()); flush(stdout())

### Name: size
### Title: Size of object
### Aliases: size

### ** Examples

x <- rnorm(20)
size(x)
# 20
x <- matrix(rnorm(100), 20, 5)
size(x)
# 20  5



cleanEx()
nameEx("table_column_attr")
### * table_column_attr

flush(stderr()); flush(stdout())

### Name: table_column_attr
### Title: Tabulate column attributes
### Aliases: table_column_attr

### ** Examples

library(data.table)
x <- data.table(
  id = 1:5,
  sbp = rnorm(5, 120, 15),
  dbp = rnorm(5, 80, 10),
  paO2 = rnorm(5, 90, 10),
  paCO2 = rnorm(5, 40, 5)
)
setattr(x[["sbp"]], "source", "outpatient")
setattr(x[["dbp"]], "source", "outpatient")
setattr(x[["paO2"]], "source", "icu")
setattr(x[["paCO2"]], "source", "icu")
table_column_attr(x, "source")



cleanEx()
nameEx("uniprot_get")
### * uniprot_get

flush(stderr()); flush(stdout())

### Name: uniprot_get
### Title: Get protein sequence from UniProt
### Aliases: uniprot_get

### ** Examples

## Not run: 
##D mapt <- uniprot_get("Q9UMX9")
## End(Not run)



cleanEx()
nameEx("uniquevalsperfeat")
### * uniquevalsperfeat

flush(stderr()); flush(stdout())

### Name: uniquevalsperfeat
### Title: Unique values per feature
### Aliases: uniquevalsperfeat

### ** Examples

## Not run: 
##D uniquevalsperfeat(iris)
## End(Not run)



cleanEx()
nameEx("xtdescribe")
### * xtdescribe

flush(stderr()); flush(stdout())

### Name: xtdescribe
### Title: Describe longitudinal dataset
### Aliases: xtdescribe

### ** Examples

## Not run: 
##D # Load example longitudinal dataset
##D data(xt_example, package = "rtemis")
##D 
##D # Describe the longitudinal structure
##D xtdescribe(xt_example)
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
