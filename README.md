# fabOF: frequency adjusted borders Ordinal Forest
## Introduction
fabOF (Buczak, 2024) is a tree ensemble method for ordinal prediction. Similar to Ordinal Forest (OF; Hornung, 2020), fabOF assigns numeric scores and category borders to the ordinal response categories and fits a regression random forest (RF; Breiman, 2001) on these numeric scores. In contrast to OF, fabOF does not require an optimization procedure and instead relies on a frequency-based heuristic to derive its category borders (for more details, see Buczak, 2024). For fitting the RF model, the `ranger` R package (Wright & Ziegler, 2017) is used.

## Installation
The fabOF R package can be installed using the `devtools` R package through the following commands
```R
library(devtools)
devtools::install_github("phibuc/fabOF") 
```

## References
* Buczak, P. (2024). fabOF: A Novel Tree Ensemble Method for Ordinal Prediction. OSF Pre-print. https://doi.org/10.31219/osf.io/h8t4p.
* Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.
* Breiman, L. (2001). Random Forests. Mach Learn, 45:5-32. https://doi.org/10.1023/A:1010933404324.
* Hornung, R. (2020). Ordinal Forests. J Classif. 37: 4-17. https://doi.org/10.1007/s00357-018-9302-x.

