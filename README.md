# Genetic Code Analysis Toolkit for R - Base package (gcatbase)

This project contains the source code for the R version of the Genetic Code Analysis Toolkit (GCAT) base project. Please refer also to the [cammbio homepage](https://www.cammbio.hs-mannheim.de/research/software/gcatbase-r) for more information.

![Genetic Code Analysis Toolkit Logo](./man/resources/bio/gcat/logo.png?raw=true)

# Installation
`gcatbase` is available for R version 4.1 or higher. It requires a [Rust](https://www.rust-lang.org/) 1.57 (or later) compiler installed on your machine. Furthermore the current version of devtools has to be installed on your computer. If you are using Microsoft Windows, then you need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

A common error is that Rust does not have the target installed.

```CMD
rustup target add [YOUR_TARGET]
```

Starting a new R console and run:
```R
install.packages("devtools")
devtools::install_github("/informatik-mannheim/gcatbase")
```

# Usage

Once you installed `gcatbase`, you may read its help pages. The file [./example/Tutorial.Rmd](./example/Tutorial.Rmd) is a good start for an introduction and a tutorial. This markdown document can be executed. The executed tutorial is available [online](https://oc.informatik.hs-mannheim.de/s/gCWxMRwfXaCcTsS/download).
