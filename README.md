This repository contains the code used to produce the empirical results of the study:

Using the Orphanhood Method to Explore the Association Between Children’s Education and Maternal Survival in Chile (https://doi.org/10.31219/osf.io/rxdqz_v1)

*Code structure*:

**00_setup**: installs/loads R packages and defines global parameters.

**01_load_clean_EPSdata**: loads and preprocesses the primary EPS (Encuesta de Protección Social) data. Includes filtering variables, cleaning missing data, and transforming variables for analysis.

**02_lifetables_data**: handles life table data needed for the Orphanhood Method. Includes importing life expectancy tables and formatting them for use in calculations.

**03_functions**: R functions used throughout the project, including functions implementing the Orphanhood Method and CI with bootstrapping.

**04_results**: generates tables and figures for the empirical results reported in the study.

*Data availability*:

1 - EPS 2002 data used in this study can be requested at: https://previsionsocial.gob.cl/datos-estadisticos/condiciones-bases-de-datos-eps/

2 - Female life tables used are from two sources:

 2.1 - Human Life Table (www.lifetable.de)

1986-1989: https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019861989CU1.txt

1990: https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019901990AU1.txt

1991-1992: https://www.lifetable.de/File/GetDocument/data/CHL/CHL000019911992AU1.txt

 2.2 - Human Mortality Database (https://www.mortality.org)

1992-2020: https://www.mortality.org/File/GetDocument/hmd.v6/CHL/STATS/fltper_5x1.txt

3 - Mean Age at Childbearing for Chile (1952-1982)

UNWPP demographic indicators: https://population.un.org/wpp
