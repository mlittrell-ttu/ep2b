# ep2b

Read all sheets from an Excel workbook into individual tibbles in your R environment, with an automatic summary of each sheet's shape and columns — in one command.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mlittrell-ttu/ep2b")
```

## Usage

```r
library(ep2b)

ep2b("path/to/your_workbook.xlsx")
```

Each sheet becomes a tibble in your environment (named after the sheet), and a formatted summary prints to the console.
