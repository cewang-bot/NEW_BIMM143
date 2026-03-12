# Lab 13_Transcriptomics and the analysis of RNA-Seq data
Cecilia Wang (A18625854)

## Background

Today we will perform an RNAseq analysis of the effects of a common
sertoid on airway cells.

In particular, dexamethasone (hereafter just called “dex”) on different
airway smooth muscle cell lines (ASM cells)

``` r
library(BiocManager)
library(DESeq2)
```

## Data import

We need to different inputs:

**countData** : with genes in rows and experiments in columns
**coldata**: metadata the describes the columns in countData

``` r
counts <- read.csv("airway_scaledcounts.csv", row.names=1)
metadata <-  read.csv("airway_metadata.csv")
```

``` r
head(counts)
```

                    SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
    ENSG00000000003        723        486        904        445       1170
    ENSG00000000005          0          0          0          0          0
    ENSG00000000419        467        523        616        371        582
    ENSG00000000457        347        258        364        237        318
    ENSG00000000460         96         81         73         66        118
    ENSG00000000938          0          0          1          0          2
                    SRR1039517 SRR1039520 SRR1039521
    ENSG00000000003       1097        806        604
    ENSG00000000005          0          0          0
    ENSG00000000419        781        417        509
    ENSG00000000457        447        330        324
    ENSG00000000460         94        102         74
    ENSG00000000938          0          0          0

``` r
head(metadata)
```

              id     dex celltype     geo_id
    1 SRR1039508 control   N61311 GSM1275862
    2 SRR1039509 treated   N61311 GSM1275863
    3 SRR1039512 control  N052611 GSM1275866
    4 SRR1039513 treated  N052611 GSM1275867
    5 SRR1039516 control  N080611 GSM1275870
    6 SRR1039517 treated  N080611 GSM1275871

> Q1. How many genes are in this dataset? Q2. How many ‘control’ cell
> lines do we have?

``` r
nrow(counts)
```

    [1] 38694

``` r
sum(metadata$dex=="control")
```

    [1] 4

## Differential gene expression

We have 4 replicate drug treated and control (no drug)
columns/experiments in our `counts` object.

We want one “mean” value for each gene (rows) in “treated” and one mean
value for each gene in “control” columns.

Find the mean number of the genes of the control group:

Step 1: Find all “control” columns

``` r
control.inds <- metadata$dex=="control"
control.inds
```

    [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE

Step 2: Extract these columns to a new object called `control.counts`

``` r
control.counts <- counts[,control.inds]
head(control.counts)
```

                    SRR1039508 SRR1039512 SRR1039516 SRR1039520
    ENSG00000000003        723        904       1170        806
    ENSG00000000005          0          0          0          0
    ENSG00000000419        467        616        582        417
    ENSG00000000457        347        364        318        330
    ENSG00000000460         96         73        118        102
    ENSG00000000938          0          1          2          0

Step 3: Then calculate the mean value for each gene

``` r
control.mean <- rowMeans(control.counts)
head(control.mean)
```

    ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
             900.75            0.00          520.50          339.75           97.25 
    ENSG00000000938 
               0.75 

Find the mean number of the genes of the treated group using the same 3
steps:

``` r
treated.inds <- metadata$dex=="treated"
treated.counts <- counts[,treated.inds]
treated.mean <- rowMeans( treated.counts )
head(treated.mean)
```

    ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 ENSG00000000460 
             658.00            0.00          546.00          316.50           78.75 
    ENSG00000000938 
               0.00 

``` r
meancounts <- data.frame(control.mean,treated.mean)
```

``` r
plot(control.mean,treated.mean)
```

![](Lab13_files/figure-commonmark/unnamed-chunk-11-1.png)

Lets log transform this count data:

``` r
plot(meancounts,log="xy")
```

    Warning in xy.coords(x, y, xlabel, ylabel, log): 15032 x values <= 0 omitted
    from logarithmic plot

    Warning in xy.coords(x, y, xlabel, ylabel, log): 15281 y values <= 0 omitted
    from logarithmic plot

![](Lab13_files/figure-commonmark/unnamed-chunk-12-1.png)

**N.B.** We most often use log2 for this type of data as it makes the
interpretation much more straightforward.

treated/control is often called “fold-change”

If there was no change we would have a log2-fc of zero:

``` r
log2(10/10)
```

    [1] 0

if we had double as much transcript:

``` r
log2(20/10)
```

    [1] 1

if we had half as much transcript:

``` r
log2(10/20)
```

    [1] -1

``` r
log2(5/20)
```

    [1] -2

> Q. Calculate a log2 fold change value for all our genes and add it as
> a new column to our `meancounts` object.

``` r
meancounts$log2f <- log2(meancounts$treated.mean/meancounts$control.mean)
head(meancounts)
```

                    control.mean treated.mean       log2f
    ENSG00000000003       900.75       658.00 -0.45303916
    ENSG00000000005         0.00         0.00         NaN
    ENSG00000000419       520.50       546.00  0.06900279
    ENSG00000000457       339.75       316.50 -0.10226805
    ENSG00000000460        97.25        78.75 -0.30441833
    ENSG00000000938         0.75         0.00        -Inf

There are some “funky” log2fc values (NaN and -Inf).

## DESeq analysis

Let’s do this analysis with an estimate of statistical significance
using the **DESeq2** package.

DeSeq2 (like many bioconductor packages) want it’s input data in a very
specific way.

``` r
dds <-DESeqDataSetFromMatrix(countData = counts,
                       colData = metadata,
                       design = ~dex)
```

    converting counts to integer mode

    Warning in DESeqDataSet(se, design = design, ignoreRank): some variables in
    design formula are characters, converting to factors

## Run the DeSeq analysis pipeline

The main function `DESeq()`

``` r
dds <- DESeq(dds)
```

    estimating size factors

    estimating dispersions

    gene-wise dispersion estimates

    mean-dispersion relationship

    final dispersion estimates

    fitting model and testing

``` r
result <- results(dds)
head(result)
```

    log2 fold change (MLE): dex treated vs control 
    Wald test p-value: dex treated vs control 
    DataFrame with 6 rows and 6 columns
                      baseMean log2FoldChange     lfcSE      stat    pvalue
                     <numeric>      <numeric> <numeric> <numeric> <numeric>
    ENSG00000000003 747.194195      -0.350703  0.168242 -2.084514 0.0371134
    ENSG00000000005   0.000000             NA        NA        NA        NA
    ENSG00000000419 520.134160       0.206107  0.101042  2.039828 0.0413675
    ENSG00000000457 322.664844       0.024527  0.145134  0.168996 0.8658000
    ENSG00000000460  87.682625      -0.147143  0.256995 -0.572550 0.5669497
    ENSG00000000938   0.319167      -1.732289  3.493601 -0.495846 0.6200029
                         padj
                    <numeric>
    ENSG00000000003  0.163017
    ENSG00000000005        NA
    ENSG00000000419  0.175937
    ENSG00000000457  0.961682
    ENSG00000000460  0.815805
    ENSG00000000938        NA

## Volcano plot

This is a main summary results figure from these kinds of studies. It is
a plot of Log2 fold-change vs. (Adjusted) P-value.

``` r
plot(result$log2FoldChange, result$padj)
```

![](Lab13_files/figure-commonmark/unnamed-chunk-19-1.png)

Again this y-axis is highly needs log transforming an we can flip the
y-axis with a minus sign so it looks like every other volcano plot.

``` r
plot(result$log2FoldChange,
     -log(result$padj))
abline(v=-2, col="red")
abline(v=2, col="red")
abline(h=-log(0.05), col="red")
```

![](Lab13_files/figure-commonmark/unnamed-chunk-20-1.png)

### Adding some color annotation

start with a default color “gray”

``` r
mycols <- rep("gray", nrow(result))
mycols[result$log2FoldChange > 2 ] <- "blue"
mycols[result$log2FoldChange < -2] <- "darkgreen"
mycols[result$padj >= 0.05 ] <- "gray"

plot(result$log2FoldChange,
     -log(result$padj),
col=mycols)

abline(v=c(-2,2), col="red", lty=2)
abline(h=-log(0.1), col="red", lty=2)
```

![](Lab13_files/figure-commonmark/unnamed-chunk-21-1.png)

Make a presentation quality ggplot version of this plot. Include clear
axis labels, a clean theme, your custom colors, cut-off lines, and a
plot

``` r
library(ggplot2)
ggplot(result)+
  aes(log2FoldChange,-log(padj))+
  geom_point(colour=mycols)+ 
  labs(x="Log2 Fold-change",
       y="-log Adjusted P-value",
       title= "Volcano Plot of Differential Gene Expression")+
  theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(face="bold",hjust=0.5),
    axis.title= element_text(face="bold"),
    legend.title= element_text(face="bold"),
    panel.grid.minor= element_blank()
  )
```

    Warning: Removed 23549 rows containing missing values or values outside the scale range
    (`geom_point()`).

![](Lab13_files/figure-commonmark/unnamed-chunk-22-1.png)

## Save Our result

Write a csv.file

``` r
#write.csv(result, file= "results.csv")
```

## Add anotation data

We need to add missing annotation data to our main `result` object. This
includes the common gene `symbol`.

``` r
head(result)
```

    log2 fold change (MLE): dex treated vs control 
    Wald test p-value: dex treated vs control 
    DataFrame with 6 rows and 6 columns
                      baseMean log2FoldChange     lfcSE      stat    pvalue
                     <numeric>      <numeric> <numeric> <numeric> <numeric>
    ENSG00000000003 747.194195      -0.350703  0.168242 -2.084514 0.0371134
    ENSG00000000005   0.000000             NA        NA        NA        NA
    ENSG00000000419 520.134160       0.206107  0.101042  2.039828 0.0413675
    ENSG00000000457 322.664844       0.024527  0.145134  0.168996 0.8658000
    ENSG00000000460  87.682625      -0.147143  0.256995 -0.572550 0.5669497
    ENSG00000000938   0.319167      -1.732289  3.493601 -0.495846 0.6200029
                         padj
                    <numeric>
    ENSG00000000003  0.163017
    ENSG00000000005        NA
    ENSG00000000419  0.175937
    ENSG00000000457  0.961682
    ENSG00000000460  0.815805
    ENSG00000000938        NA

We will use R and bioconductor to do this “ID mapping”

``` r
library("AnnotationDbi")
library("org.Hs.eg.db")
```

Let’s see what databases we can use for translation/mapping…

``` r
columns(org.Hs.eg.db)
```

     [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT"  "ENSEMBLTRANS"
     [6] "ENTREZID"     "ENZYME"       "EVIDENCE"     "EVIDENCEALL"  "GENENAME"    
    [11] "GENETYPE"     "GO"           "GOALL"        "IPI"          "MAP"         
    [16] "OMIM"         "ONTOLOGY"     "ONTOLOGYALL"  "PATH"         "PFAM"        
    [21] "PMID"         "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG"      
    [26] "UNIPROT"     

We can use the `mapIds()` function now to “translate” between any of
these databases.

``` r
result$symbol<-mapIds(org.Hs.eg.db,
                     keys=row.names(result), # Our genenames
                     keytype="ENSEMBL",        # The format of our genenames
                     column="SYMBOL",          # The new format we want to add
                     multiVals="first")
```

    'select()' returned 1:many mapping between keys and columns

> Q. Also add “ENTREZID”,“GENENAME”

``` r
result$entrez<-mapIds(org.Hs.eg.db,
                     keys=row.names(result), # Our genenames
                     keytype="ENSEMBL",        # The format of our genenames
                     column="ENTREZID",          # The new format we want to add
                     multiVals="first")
```

    'select()' returned 1:many mapping between keys and columns

``` r
result$genename <- mapIds(org.Hs.eg.db,
                     keys=row.names(result), # Our genenames
                     keytype="ENSEMBL",        # The format of our genenames
                     column="GENENAME",          # The new format we want to add
                     multiVals="first")
```

    'select()' returned 1:many mapping between keys and columns

``` r
head(result)
```

    log2 fold change (MLE): dex treated vs control 
    Wald test p-value: dex treated vs control 
    DataFrame with 6 rows and 9 columns
                      baseMean log2FoldChange     lfcSE      stat    pvalue
                     <numeric>      <numeric> <numeric> <numeric> <numeric>
    ENSG00000000003 747.194195      -0.350703  0.168242 -2.084514 0.0371134
    ENSG00000000005   0.000000             NA        NA        NA        NA
    ENSG00000000419 520.134160       0.206107  0.101042  2.039828 0.0413675
    ENSG00000000457 322.664844       0.024527  0.145134  0.168996 0.8658000
    ENSG00000000460  87.682625      -0.147143  0.256995 -0.572550 0.5669497
    ENSG00000000938   0.319167      -1.732289  3.493601 -0.495846 0.6200029
                         padj      symbol      entrez               genename
                    <numeric> <character> <character>            <character>
    ENSG00000000003  0.163017      TSPAN6        7105          tetraspanin 6
    ENSG00000000005        NA        TNMD       64102            tenomodulin
    ENSG00000000419  0.175937        DPM1        8813 dolichyl-phosphate m..
    ENSG00000000457  0.961682       SCYL3       57147 SCY1 like pseudokina..
    ENSG00000000460  0.815805       FIRRM       55732 FIGNL1 interacting r..
    ENSG00000000938        NA         FGR        2268 FGR proto-oncogene, ..

## Save annotated results to a csv file

``` r
write.csv(result,file="results_annotated.csv")
```

## Pathway analysis

What known biological pathways do our differentially expressed genes
overlap with (i.e. play a role in )

There are lot’s of bioconductor packages to do this type of analysis.

We will use one of the oldest called **gage** and with **pathview** to
render nice pics of the pathways we find.

``` r
library(pathview)
library(gage)
library(gageData)
```

Have a wee peak what is in the `gageData`

``` r
data(kegg.sets.hs)

# Examine the first 2 pathways in this kegg set for humans
head(kegg.sets.hs, 2)
```

    $`hsa00232 Caffeine metabolism`
    [1] "10"   "1544" "1548" "1549" "1553" "7498" "9"   

    $`hsa00983 Drug metabolism - other enzymes`
     [1] "10"     "1066"   "10720"  "10941"  "151531" "1548"   "1549"   "1551"  
     [9] "1553"   "1576"   "1577"   "1806"   "1807"   "1890"   "221223" "2990"  
    [17] "3251"   "3614"   "3615"   "3704"   "51733"  "54490"  "54575"  "54576" 
    [25] "54577"  "54578"  "54579"  "54600"  "54657"  "54658"  "54659"  "54963" 
    [33] "574537" "64816"  "7083"   "7084"   "7172"   "7363"   "7364"   "7365"  
    [41] "7366"   "7367"   "7371"   "7372"   "7378"   "7498"   "79799"  "83549" 
    [49] "8824"   "8833"   "9"      "978"   

The main `gage()` function that does the work wants a simple vector as
input.

``` r
foldchanges = result$log2FoldChange

#The KEGG data base uses ENTREZ ids so we need to provide these in our input vector for *gage()*

names(foldchanges) = result$entrez
head(foldchanges)
```

           7105       64102        8813       57147       55732        2268 
    -0.35070296          NA  0.20610728  0.02452701 -0.14714263 -1.73228897 

Now, let’s run the gage pathway analysis.

``` r
# Get the results
keggres = gage(foldchanges, gsets=kegg.sets.hs)
```

What is in the output object `keggres`

``` r
attributes(keggres)
```

    $names
    [1] "greater" "less"    "stats"  

``` r
# Look at the first three down (less) pathways
head(keggres$less, 3)
```

                                          p.geomean stat.mean        p.val
    hsa05332 Graft-versus-host disease 0.0004250607 -3.473335 0.0004250607
    hsa04940 Type I diabetes mellitus  0.0017820379 -3.002350 0.0017820379
    hsa05310 Asthma                    0.0020046180 -3.009045 0.0020046180
                                            q.val set.size         exp1
    hsa05332 Graft-versus-host disease 0.09053792       40 0.0004250607
    hsa04940 Type I diabetes mellitus  0.14232788       42 0.0017820379
    hsa05310 Asthma                    0.14232788       29 0.0020046180

We can use the **pathview** function to render a figure of any of these
pathways along with annotations for our DEGs.

``` r
pathview(gene.data=foldchanges, pathway.id="hsa05310")
```

    'select()' returned 1:1 mapping between keys and columns

    Info: Working in directory /Users/yushiwang/BIMM 143 R /Class 13/Class 13

    Info: Writing image file hsa05310.pathview.png

![](hsa05310.pathview.png)

> Q. Can you render the pathway analysis for Graft-versus-host disease
> and Type I diabetes mellitus?

``` r
#Graft-versus-host disease
pathview(gene.data=foldchanges, pathway.id="hsa05332")
```

    'select()' returned 1:1 mapping between keys and columns

    Info: Working in directory /Users/yushiwang/BIMM 143 R /Class 13/Class 13

    Info: Writing image file hsa05332.pathview.png

![](hsa05332.pathview.png)

``` r
#Type I diabetes mellitus
pathview(gene.data=foldchanges, pathway.id="hsa04940 ")
```

    Info: Downloading xml files for hsa04940 , 1/1 pathways..

    Warning in download.file(xml.url, xml.target, quiet = T): URL
    'https://rest.kegg.jp/get/hsa04940 /kgml': status was 'URL using bad/illegal
    format or missing URL'

    Warning: Download of hsa04940  xml file failed!
    This pathway may not exist!

    Info: Downloading png files for hsa04940 , 1/1 pathways..

    Warning: Download of hsa04940  png file failed!
    This pathway may not exist!

    Warning: Failed to download KEGG xml/png files, hsa04940  skipped!

The pathway for Type I diabetes mellitus may not exist.
