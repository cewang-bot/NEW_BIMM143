# Class 11: Structural Bioinformatics (pt2. Focus on new AlphaFold2)
Cecilia Wang (A18625854)

## Background

We saw last day that the main repository for biomolecular structure (the
PDB database) only has 250,000 entries.

UniprotKB (the main protein sequence database) has over 200 million
entries!

In this hands-on session we will utilize Alphafold to pridict protein
strutcure from sequence (Jumper et al. 2021).

Without the aid of such approahes, it can take years of expensive
labortory work to determine the structure of just one protein. With
Alphafold, we ca now accurately compute a typical protein structure in
as little as ten mins.

## The EBI AlphaFold database

The EBI alphafold data base cintains a lot of computed structure models.
It is uncreasingly likely that the structure you are interested in is
already in this database at <https://alphafold.ebi.ac.uk/>

There are 3 major outputs:

1.  A model of structure in **PDB** format.
2.  A **PLDDT score** that tells us how confident the model is for a
    given residue in your protein (High values are good, above 70)
3.  a **PAE score** that tells us about protein packaging quality.

If you can’t find a matching entry for the sequence you are interested
in AFDB you can run AlphaFold yourself.

## Running AlphaFold

We will use ColabFold to run AlphaFold

## Interpreting Results

Custom analysis of resulting models

we can read all the AlphaFold results into R and do more quantitive
analysis tha just viewing the structures in Mol-Star

Read all the PDB models:

``` r
library(bio3d)
pdb_files <- list.files("hivpr_23119", pattern=".pdb", full.names=T)
pdbs <- pdbaln(pdb_files, fit=TRUE, exefile="msa")
```

    Reading PDB files:
    hivpr_23119/hivpr_23119_unrelaxed_rank_001_alphafold2_multimer_v3_model_4_seed_000.pdb
    hivpr_23119/hivpr_23119_unrelaxed_rank_002_alphafold2_multimer_v3_model_1_seed_000.pdb
    hivpr_23119/hivpr_23119_unrelaxed_rank_003_alphafold2_multimer_v3_model_5_seed_000.pdb
    hivpr_23119/hivpr_23119_unrelaxed_rank_004_alphafold2_multimer_v3_model_2_seed_000.pdb
    hivpr_23119/hivpr_23119_unrelaxed_rank_005_alphafold2_multimer_v3_model_3_seed_000.pdb
    .....

    Extracting sequences

    pdb/seq: 1   name: hivpr_23119/hivpr_23119_unrelaxed_rank_001_alphafold2_multimer_v3_model_4_seed_000.pdb 
    pdb/seq: 2   name: hivpr_23119/hivpr_23119_unrelaxed_rank_002_alphafold2_multimer_v3_model_1_seed_000.pdb 
    pdb/seq: 3   name: hivpr_23119/hivpr_23119_unrelaxed_rank_003_alphafold2_multimer_v3_model_5_seed_000.pdb 
    pdb/seq: 4   name: hivpr_23119/hivpr_23119_unrelaxed_rank_004_alphafold2_multimer_v3_model_2_seed_000.pdb 
    pdb/seq: 5   name: hivpr_23119/hivpr_23119_unrelaxed_rank_005_alphafold2_multimer_v3_model_3_seed_000.pdb 

``` r
#library(bio3dview)
#view.pdbs(pdbs)
```

How similar or different are my models?

``` r
rd <- rmsd(pdbs, fit=T)
```

    Warning in rmsd(pdbs, fit = T): No indices provided, using the 198 non NA positions

``` r
library(pheatmap)

colnames(rd) <- paste0("m",1:5)
rownames(rd) <- paste0("m",1:5)
pheatmap(rd)
```

![](Class11_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
# Read a reference PDB structure
pdb <- read.pdb("1hsg")
```

      Note: Accessing on-line PDB file

``` r
plotb3(pdbs$b[1,], typ="l", lwd=2, sse=pdb)
points(pdbs$b[2,], typ="l", col="red")
points(pdbs$b[3,], typ="l", col="blue")
points(pdbs$b[4,], typ="l", col="darkgreen")
points(pdbs$b[5,], typ="l", col="orange")
abline(v=100, col="gray")
```

![](Class11_files/figure-commonmark/unnamed-chunk-5-1.png)
