## The Limits of Human Predictions of Recidivism

This is the replication code repository for analysis in paper _The Limits of Human Predictions of Recidivism_.

### Run Analysis
To view tables and figures generated by us, `results.html` contains all figures and tables in the paper.

To rerun all the analysis in the paper, you may execute `Rscript run.R` in command line.

Runing our analysis scripts requires several `R` packages. They are being specified at the top of `src/pkgs.R` file.
You can install them via running the following command:
```
install.packages(
  c("scales", "matrixStats", "boot", "arm", "ROCR", "assertthat", "doMC", 
  "forcats", "furrr", "lubridate",  "kableExtra", "tidyverse")
 )
```

After running the script, you can view the result by running/knitting `results.Rmd`.

### LSI-R Dataset
The original LSI-R datasets under `data/private/individuals/` are only available upon request due to privacy concerns.
However, you can still run the analysis using our pre-generated intermediate data.

If the original LSI-R datasets are not available, ID and outcome columns from the LSI-R data (under `data/public/individuals/`) and pre-calculated summary statistics (under `data/public/derived/lsi`) are used to reproduce the results.
If the original LSI-R datasets do exist under `data/private/individuals/`, then derived data at `data/public/derived/lsi` will be recalculated using the original datasets.

### Reference
Lin, Zhiyuan "Jerry", Jongbin Jung, Sharad Goel, Jennifer Skeem. "The Limits of Human Predictions of Recidivism." Science Advances (2020).

BibTex
```
@article{lin2020limits,
  title={The Limits of Human Predictions of Recidivism},
  author={Lin, Zhiyuan "Jerry" and Jung, Jongbin and Goel, Sharad and Skeem, Jennifer},
  journal={Science Advances},
  year={2020},
  publisher={American Association for the Advancement of Science}
}
```