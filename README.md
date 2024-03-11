MARCviz: A package for Meta-Analytic Rain-Cloud Plots
======================================
[![License: GPL (>=3)](https://img.shields.io/badge/license-GPL-blue)](https://www.gnu.org/licenses/gpl-3.0.txt)

# Description

The `MARCviz` package contains the function `viz_MARC` for creating the
Meta-Analytic Rain Cloud (MARC) Plot, from [Fitzgerald & Tipton
(2022)](https://www.tandfonline.com/doi/abs/10.1080/19345747.2022.2031366).

**Note, this package is still under construction!**

# Installation

To install the package, run the following code:

```r
devtools::install_github("kgfitzgerald/MARCviz")
```

# Example 

Once installed, you can load the package and pull up the help
documentation for the `viz_MARC` function.

```r
library(MARCviz)
?viz_MARC
```

Read data:

```r
library(tidyverse)
#read in meta-analytic dataset
data(viz_MA_data)

#specify vector of effect sizes
d_j <- viz_MA_data %>% filter(k == 100) %>% pull(d_j)
#specify vector of standard errors
se_j <- viz_MA_data %>% filter(k == 100) %>% pull(se_j)
```

Produce MARC plot (plotly - default)

```r
#create MARC plot (default is interactive plotly object)
viz_MARC(d_j, se_j)
```

Produce MARC plot (ggplot)

```r
#create static MARC plot (ggplot object)
viz_MARC(d_j, se_j, type = "static")
```

# License, Citation, Issues

The `MARCviz` package was written by Kaitlyn G. Fitzgerald and Avery Charles. It is licensed under the [GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.txt). For citation info, type `citation(package='MARCviz')` in R. To report any issues or bugs or to suggest enhancements to the package, please go [here](https://github.com/kgfitzgerald/MARCviz/issues).
