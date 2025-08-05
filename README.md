The PDF document is avaliable here: https://github.com/bttomio/UGA_thesisdown/blob/main/docs/_main.pdf.

To reproduce the thesis manuscript and related materials, please follow the steps below:

First, download the code as a `.zip` file and extract the contents to a specific folder.

With the files in this repository, you can:

1. Reproduce the thesis manuscript

* Open `bttomio-thesis.Rproj`
* Open `index.Rmd`
* Knit (Ctrl + Shitf + k) `index.Rmd`
* Note: It may take some time to knit the file.
* The final document `_main.pdf`, will be located in the `docs` folder

2. Reproduce the econometric results (Stata and R codes)

* Navigate to the `ECONOMETRIC PROCEDURES` directory.
* For Chapter 4, use `BGVAR_2.4.3`(https://github.com/mboeck11/BGVAR).

3. Reproduce the thesis committee presentation

* View the presentation here: https://bttomio.github.io/slides/PhDdefense/slides.html
* Access the source code: https://github.com/bttomio/bttomio.github.io/tree/master/slides/PhDdefense

`sessionInfo()` output for the manuscript:

```
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] colorspace_2.0-2 scales_1.1.1     compiler_4.1.2   R6_2.5.1         bookdown_0.24   
 [6] fastmap_1.1.0    htmltools_0.5.2  tools_4.1.2      yaml_2.2.1       rmarkdown_2.11  
[11] knitr_1.36.9     xfun_0.28.10     digest_0.6.29    lifecycle_1.0.1  munsell_0.5.0   
[16] rlang_0.4.12     evaluate_0.14
```
