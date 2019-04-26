Extract Shen 278 ROIs
========================================================
author: Zikai Lin
date: 04/25/2019
autosize: true

Idea - Why This Application?
========================================================

One of the most commonly used functional brain atlas is [Shen et al Neuroimage 2013] <https://www.sciencedirect.com/science/article/pii/S1053811913005818?via%3Dihub>. 

However,

- Contain 278 brain functional region of interests (ROI)
- Related materials (nii files) can be downloaded <https://www.nitrc.org/frs/?group_id=51>
- Not designed in R environment.
- Need Matlab, AFNI or FSL to extract the information.

Therefore, I designed this application to make this parcellation usable in R, or users that is not familiar with R.

Required libraries
========================================================


```r
require(oro.dicom)
require(oro.nifti)
require(mritc)
library(scales)
library(RNifti)
library(neurobase)
```

These are required R packages for hacking neuroimage in R.


Sample Brain Parcellation Plot
========================================================


```r
load(file = "../Shen_MNI152_ORG.RData")
oro.nifti::orthographic(shen_MNI152_org)
```

![plot of chunk unnamed-chunk-2](locateShenROIs_documentation-figure/unnamed-chunk-2-1.png)

The parcellation can be visulized in this application, and it will automatically be updated once the user enter new coordinates.

Thank You!
========================================================

You can view the source code in my github repo: <https://github.com/proteuslinzk/locateShenROIs>.
