# Gender Wage Gap in Chile — Master's Project

Replication code for a master's project analysing the gender wage gap in Chile using the **CASEN 2022** survey (*Encuesta de Caracterización Socioeconómica Nacional*).

---

## Repository structure

| File | Description |
|---|---|
| `Code.R` | Main analysis script — data cleaning, descriptive stats, PCA, clustering, Tobit, Oaxaca-Blinder |
| `Gender wage gap.Rmd` | Full report in R Markdown — combines the analysis with interpretation and figures |

## How to run

1. Download the CASEN 2022 data from the [Ministerio de Desarrollo Social y Familia](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022) and place the file in a `data/` folder at the project root:
   ```
   data/CASEN 2022.dta
   ```
2. Open the project in RStudio (or set the working directory to the project root)
3. Run `Code.R` for the full analysis, or knit `Gender wage gap.Rmd` for the report

> **Note:** The CASEN 2022 file is approximately 1.6 GB and cannot be hosted in this repository. It is freely available from the link above.

## Data

The analysis uses a single file:
- `CASEN 2022.dta` — CASEN 2022 household survey (Stata format), ~200,000 individuals

## Methodology

1. **Sample selection** — active population following ILO definition (employed + unemployed, age ≥ 15)
2. **Descriptive statistics** — employment, education, and health variables by gender
3. **PCA** — composite variables for work quality, education, and health
4. **K-means clustering** — identify homogeneous groups
5. **Tobit II model** — wage equation correcting for labor force participation selection bias
6. **Oaxaca-Blinder decomposition** — decompose the wage gap into explained and unexplained components

## Dependencies

All R packages are loaded automatically via `pacman::p_load()` at the top of each file. The main packages used are `haven`, `here`, `dplyr`, `ggplot2`, `FactoMineR`, `sampleSelection`, and `oaxaca`.
