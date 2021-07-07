
# dragon
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html) 
[![Build Status](https://travis-ci.org/sjspielman/dragon.svg?branch=master)](https://travis-ci.org/sjspielman/dragon)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dragon)](https://CRAN.R-project.org/package=dragon)

**The current version is 1.2.0.**
_Note that the license was changed to MIT with the release of 1.2.0 for compatibility with dependencies._


<img src="inst/app/www/favicon.ico" height="250"/>



The `dragon` (**D**eep time **R**edox **A**nalysis of the **G**eobiology **O**ntology **N**etwork) package provides a [Shiny Application](https://shiny.rstudio.com/) for generating, exploring, and analyzing bipartite mineral-chemistry networks over deep time on Earth using information from the [Mineral Evolution Database](https://rruff.info/ima/), with a specific application of investigating biologically-relevant evolution of element redox states and availability over time. `dragon` uses `igraph` and `visNetwork` library (a terribly handy R wrapper for `vis.js`) to construct user-friendly interactive networks. 

**For instructions on obtaining and using `dragon`**, please see the [vignette](http://htmlpreview.github.io/?https://github.com/sjspielman/dragon/blob/master/doc/dragon.html).

**Current link to server of current release of `dragon`:** [https://sjspielman.shinyapps.io/dragon](https://sjspielman.shinyapps.io/dragon)

**The associated publication** is available [here](https://doi.org/10.3389/feart.2020.585087) and can be cited as:

Spielman, Stephanie J. and Moore, Eli K. 2020. “dragon: A New Tool for Exploring Redox Evolution Preserved in the Mineral Record.” *Frontiers in Earth Science*. doi:10.3389/feart.2020.585087.

Or, in LaTeX:
```
@article{  ,
  author  = {Spielman, Stephanie J. and Moore, Eli K.},   
  title   = {dragon: A New Tool for Exploring Redox Evolution Preserved in the Mineral Record},      
	journal = {Frontiers in Earth Science},      
	volume  = {8},    
	pages   = {414},
	year    = {2020},
	doi     = {10.3389/feart.2020.585087}
}
```

**Please also cite the Mineral Evolution Database if you are using `dragon`**:

Golden, Joshua J., Downs, Robert T., Hazen, Robert M., Pires, Alexander J., Ralph, Jolyon. 2019. Mineral Evolution Database: Data-Driven Age Assignment, How Does a Mineral Get an Age?” *In GSA Annual Meeting. Phoenix, Arizona.* [https://doi.org/10.1130/abs/2019AM-334056](https://doi.org/10.1130/abs/2019AM-334056).

Or, in LaTeX:
```
@inproceedings{ ,
	location = {Phoenix, Arizona},
	title = {Mineral Evolution Database: Data-Driven Age Assignment, How Does a Mineral Get an Age?},
	url = {https://doi.org/10.1130/abs/2019AM-334056},
	booktitle = {{GSA} Annual Meeting},
	year = 2019,
	author = {Golden, Joshua J. and Downs, Robert T. and Hazen, Robert M. and Pires, Alexander J. and Ralph, Jolyon}
}
```

<!--
**Current link to server of development and wholly unguaranteed version of `dragon`:** [https://sjspielman.shinyapps.io/dragon-dev](https://sjspielman.shinyapps.io/dragon-dev)
-->

