# fabOF: Frequency-Adjusted Borders Ordinal Forest
## Introduction
Frequency-Adjusted Borders Ordinal Forest (fabOF; Buczak, 2024a) is a tree ensemble method for ordinal prediction. Similar to Ordinal Forest (OF; Hornung, 2020), fabOF assigns numeric scores and category borders to the ordinal response categories and fits a regression random forest (RF; Breiman, 2001) on these numeric scores. In contrast to OF, fabOF does not require an optimization procedure and instead relies on a frequency-based heuristic to derive its category borders (for more details, see Buczak, 2024). For fitting the RF model, the `ranger` R package (Wright & Ziegler, 2017) is used. For ordinal prediction with hierarchical data, Mixed-Effects Frequency-Adjusted Borders Ordinal Forest (mixfabOF; Buczak, 2024b) is implemented.

## Installation
The fabOF R package can be installed using the `devtools` R package through the following commands
```R
library(devtools)
devtools::install_github("phibuc/fabOF") 
```

## References
* Buczak, P. (2024a). fabOF: A Novel Tree Ensemble Method for Ordinal Prediction. OSF Pre-print. https://doi.org/10.31219/osf.io/h8t4p.
* Buczak, P. (2024b). Mixed-Effects Frequency-Adjusted Borders Ordinal Forest: A Tree Ensemble Method for Ordinal Prediction with Hierarchical Data. OSF Pre-print. https://doi.org/10.31219/osf.io/ny6we.
* Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.
* Breiman, L. (2001). Random Forests. Mach Learn, 45:5-32. https://doi.org/10.1023/A:1010933404324.
* Hornung, R. (2020). Ordinal Forests. J Classif. 37: 4-17. https://doi.org/10.1007/s00357-018-9302-x.

