# lab 12_Part 2
Cecilia Wang (A18625854)

Read the file

``` r
url <- "https://bioboot.github.io/bggn213_W19/class-material/rs8067378_ENSG00000172057.6.txt"
data <- read.table(url, header = TRUE, sep = "", quote = "", fill = TRUE)
```

> Q13. Determine the sample size for each genotype and their
> corresponding median expression levels for each of these genotypes.

``` r
table(data$geno)
```


    A/A A/G G/G 
    108 233 121 

``` r
tapply(data$exp, data$geno, median)
```

         A/A      A/G      G/G 
    31.24847 25.06486 20.07363 

> Q14: Generate a boxplot with a box per genotype, what could you infer
> from the relative expression value between A/A and G/G displayed in
> this plot? Does the SNP effect the expression of ORMDL3?

``` r
library(ggplot2)

ggplot(data, aes(x = geno, y = exp, fill = geno)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
  labs(
    title = "ORMDL3 Expression by Genotype",
    x = "Genotype",
    y = "ORMDL3 Expression Level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Lab12_files/figure-commonmark/unnamed-chunk-3-1.png)

From the plot, we can see that the A\|A samples have higher ORMDL3
expression level than G\|G samples. There is a correlation between SNP
and expression of ORMDL3.
