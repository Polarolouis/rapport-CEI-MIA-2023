#!/usr/bin/env Rscript
require("knitr")
create_latex <- function(f) {
    knitr::knit(f, "/tmp/tmp-outputfile.md")
    newname <- paste0(tools::file_path_sans_ext(f), ".tex")
    mess <- paste("pandoc --extract-media=./img -f markdown -t latex -p -o", shQuote(newname), "/tmp/tmp-outputfile.md")
    system(mess)
}
args <- commandArgs(trailingOnly = TRUE)

create_latex(args)