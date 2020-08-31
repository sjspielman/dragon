
# dragon

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable) 
[![Build Status](https://travis-ci.org/sjspielman/dragon.svg?branch=master)](https://travis-ci.org/sjspielman/dragon)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dragon)](https://CRAN.R-project.org/package=dragon)


<img src="inst/app/www/favicon.ico" height="250"/>




The `dragon` (**D**eep time **R**edox **A**nalysis of the **G**eobiology **O**ntology **N**etwork) package provides a [Shiny Application](https://shiny.rstudio.com/) for generating, exploring, and analyzing bipartite mineral-chemistry networks over deep time on Earth using information from the [Mineral Evolution Database](https://rruff.info/ima/), with a specific application of investigating biologically-relevant evolution of element redox states and availability over time. `dragon` uses `igraph` and `visNetwork` library (a terribly handy R wrapper for `vis.js`) to construct user-friendly interactive networks. 

**The associated pre-print** is available [here](https://eartharxiv.org/z7k9q/) and can be cited as:

Stephanie J. Spielman and Eli K. Moore. 2020. “dragon: A New Tool for Exploring Redox Evolution Preserved in the Mineral Record.” EarthArXiv. July 16. doi:10.31223/osf.io/z7k9q.

Or, in LaTeX:
```
@article{  ,
 journal = {EarthArXiV},
 title = {dragon: A New Tool for Exploring Redox Evolution Preserved in the Mineral Record},
 year = 2020,
 doi = {10.31223/osf.io/z7k9q}
}
```

**For instructions on obtaining and using dragon**, please see the [vignette](http://htmlpreview.github.io/?https://github.com/sjspielman/dragon/blob/master/doc/dragon.html).

**Current link to server of current release of dragon:** [https://sjspielman.shinyapps.io/dragon](https://sjspielman.shinyapps.io/dragon)


**Current link to server of development version of dragon:** [https://sjspielman.shinyapps.io/dragon-dev](https://sjspielman.shinyapps.io/dragon-dev)


