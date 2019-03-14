# RouaultSeowGillanFleming

This repository contains analysis code for the following paper:

Rouault, Seow, Gillan & Fleming (2018) Psychiatric symptom dimensions are associated with dissociable shifts in metacognition but not task performance. Biological Psychiatry, in press. doi: 10.1016/j.biopsych.2017.12.017

Anonymised behavioral data files are included in the repository to enable replication of data analyses and rapid generation of the figures in the paper.

The file regressions.R will reproduce the panels of the figures in the paper by performing the regressions and the factor analysis on Experiment 2, loading in .mat and .csv files containing relevant behavioral data:
- The metacognition task data for all subjects: ME_phase2_excludanalyseddata_all.mat.
- The questionnaire data for all subjects: ME_phase2_excludqnadata_all.mat.
- The fitted HDDM parameters for all subjects: subjParams_2k_3chain.csv.

We also provide for Experiment 1:
- The metacognition task data for all subjects: ME_phase1_excludanalyseddata_all.mat.
- The questionnaire data for all subjects: ME_phase1_excludqnadata_all.mat.

The paths in this script require altering to point to the relevant folder in your local version of the repository. You would alter:

```
setwd("/Users/marion/Desktop/PDOC/WebStudyMetacog/Data/ExperimentII")
```

to

```
setwd("/pathToGitHubRepo/Data/ExperimentII")
```

We make use of the following packages under R:

- [lme4](http://cran.r-project.org/web/packages/lme4)
- [doBy](https://cran.r-project.org/web/packages/doBy/index.html)
- [R.matlab](https://cran.r-project.org/web/packages/R.matlab/index.html)

License

This code is being released with a permissive open-source license. You should feel free to use or adapt the utility code as long as you follow the terms of the license, which are enumerated below. If you make use of or build on the behavioral analyses, we would appreciate that you cite the paper.

Copyright (c) 2018, Marion Rouault

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


