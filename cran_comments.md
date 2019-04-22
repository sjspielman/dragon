## Test environments
* Local OS X install, R 3.5.3
* Linux (x86_64-pc-linux-gnu on travis-ci), R versions `oldrel`, `release`, and `devel`
* Windows versions `devel` and `release` 

## R CMD check results 

### OS X

`0 errors ✔ | 0 warnings ✔ | 0 notes ✔`


### Linux 

Status: OK

`Done. Your build exited with 0.`

### windows devel (3.6.0 alpha)

Status: 1 NOTE


`* checking CRAN incoming feasibility ... NOTE`
`Maintainer: 'Stephanie J. Spielman <spielman@rowan.edu>'`

### windows release (3.5.3)

Status: 1 NOTE


`* checking CRAN incoming feasibility ... NOTE`
`Maintainer: 'Stephanie J. Spielman <spielman@rowan.edu>'`

## Additional Notes

This comment was noted from initial CRAN submission (on 4/11/19) and has been fixed in current CRAN submission: 
> Author(s): Stephanie J. Spielman [aut, cre] 
> Maintainer: Stephanie J. Spielman <spielman@rowan.edu> 
> Suggests: knitr 
> Description: Shiny application for network visualization and manipulation 
> of the mineral-chemistry network across deep time on Earth. 
>
>Please don't capitalize Earth. 
>
>Please fix and resubmit. 
>
>Best, 
>Matthias Sterrer 
