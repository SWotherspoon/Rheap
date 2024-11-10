
# Rheap

<!-- badges: start -->
<!-- badges: end -->

Rheap provides a minimal implementation of min and max heaps in R.

## Installation

The package is easily installed from GitHub, using the devtools package. 

```r
devtools::install_github("SWotherspoon/Rheap")
```

If you don't have `devtools` installed already, install it first. 

```r
install.packages("devtools")
```

(Rheap otherwise does not need devtools for normal use.)

## Example

To find the tenth largest value in an unsorted vector, we can use a max-heap:
```r
library(Rheap)
x <- runif(1000)
## Find tenth largest value in x
heap <- npushpop(maxheap(x[1:10]), x[-(1:10)])
heap[1]
sort(x)[10]
```

