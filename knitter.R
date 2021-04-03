library(knitr)
library(rmarkdown)
setwd()
knit(input='main_real_estate_notebook_code.Rmd')
render(input='main_real_estate_notebook_code.md',output_format='html_document')

