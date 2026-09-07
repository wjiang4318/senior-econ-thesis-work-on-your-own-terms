# Work on Your Terms?

This was my senior thesis work in economics at Hamilton College, May 2025. You
can read the full thesis [here](docs/Wilson_Jiang_560_Thesis.pdf).

It looks at how nonstandard work arrangements (independent contractors, contract
firm employees, on-call workers) relate to job satisfaction and happiness, using
the General Social Survey from 2006 to 2022. I did the cleaning and exploratory
work in R, and the regressions in Stata.

## What I found

Of the three alternative work arrangements, only
independent contractors are noticeably happier with their jobs than regular
employees, and a bit happier in general. The boost in general happiness seems to 
flow through job satisfaction, which may reflect benefits like autonomy and schedule control

On-call workers are the opposite case. They are clearly less satisfied with their
jobs, but no less happy overall. Contract firm employees look about the same as
regular employees on both.

## Repo

```
R/           cleaning, figures, tables    (run_all.R runs everything)
stata/       the regressions
notebooks/   occupation coding
data/        raw inputs, processed output
output/      figures and tables
docs/        the thesis
```

```r
source("R/run_all.R")
```
