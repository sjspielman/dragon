## Test environments
* Local OS X install, R 3.5.3
* Linux (x86_64-pc-linux-gnu on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results 

### OS X

`0 errors ✔ | 0 warnings ✔ | 0 notes ✔`


### Linux 

Status: OK

`The command "grep -q -R "WARNING" "${RCHECK_DIR}/00check.log"" exited with 0.`

### windows devel

Status: 1 NOTE


`* checking CRAN incoming feasibility ... NOTE`
`Maintainer: 'Stephanie J. Spielman <spielman@rowan.edu>'`

### windows release

Status: 1 NOTE


`* checking CRAN incoming feasibility ... NOTE`
`Maintainer: 'Stephanie J. Spielman <spielman@rowan.edu>'`


