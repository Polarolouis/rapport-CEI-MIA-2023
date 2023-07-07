#!/usr/bin/Rscript

print(getwd())

options(knitr.table.format = "latex")
create_latex <- function(f) {
    knitr::knit(f, "/tmp/tmp-outputfile.md")
    newname <- paste0(tools::file_path_sans_ext(f), ".tex")
    mess <- paste("pandoc --extract-media=./img -f markdown -t latex -p /tmp/tmp-outputfile.md -o", shQuote(newname))
    system(mess)
}
args <- commandArgs(trailingOnly = TRUE)

create_latex(unlist(args))
