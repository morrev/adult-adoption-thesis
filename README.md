adult-adoption-thesis
==========

An exploration of factors underlying the comparatively high adult adoption rate in Japan.
Undergraduate thesis, with data cleaning/analysis in R and Stata.

Contents (Documentation):
----
```
- Data
  - Importable
    - data files…
  - Original
    - data files…
- Do-files
  - import.R
  - results.do
- Metadata
  - metadata.pdf
```

Description of Contents:
----
### `Data`:
The list and description of data files used can be found in the `metadata.txt` file. These data files are located in the Importable and Original folders under the `Data` folder.

Most of the files in `Data/Original` have been kept unchanged in the `Data/Importable` data folder, with the exception of:
- original_elderlycare2006to2016.xlsx
- original_primaryindustryGDP2001to2014.csv
- original_taxincome20**.xls

These files have been manually re-organized due to inconsistencies in sheet numbers across the taxincome data files, and in formatting across the elderly care files. The data itself are left unchanged in all cases.

### Do-files:
The importing and cleaning of data files are performed in the `import.R` file, which reads in the files from `Importable`, extracts and cleans the relevant variables (e.g. removing commas, dashes in Japanese encoding, etc.),  and merges all data into a single .dta file.
This .dta file, created by running `import.R`, is saved to the `Do-file` folder as `final.dta`.
Basic descriptions of the cleaning and merging process can be found as line comments in the `import.R` file.

The other file in the Do-files folder, `results.do` produces the regression tables presented in the paper, saving them as FinalReg1.doc, FinalReg1b.doc, FinalReg2.doc, FinalReg2b.doc, and FinalReg3.doc.
The regressions presented in the paper are FinalReg1.doc, FinalReg2.doc, and FinalReg3.doc; the remaining regressions, which use regional fixed effects, are presented in the appendix of the paper.

### Metadata:
Contains “metadata.pdf” with links to and descriptions of the data used in the empirical section of the paper.
