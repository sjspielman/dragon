## Major version update
This submission is a major version update (1.0.0) of the package `dragon`, currently on CRAN as version 0.2.1.


## Test environments
* OS X local build for R version 3.6.3
* Linux Ubuntu 18.04 on travis-ci (R versions `release`, `devel`, `oldrelease`)
* Windows 10 local builds (R versions `release`, `devel`, `oldrelease`) 

## R CMD check results 

### OS X

#####  Local build
```
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5
```

R CMD CHECK completed with:
```
── R CMD check results ─────────────────────────────────────── dragon 0.3.0 ────
Duration: 1m 2.3s
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```


### Linux 


##### release
```
R version 4.0.0 (2020-04-24)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS
```

R CMD CHECK completed with:

```
Status: 1 NOTE

* checking package dependencies ... NOTE
Imports includes 31 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally.
```

##### oldrelease
```
R version 3.6.3 (2017-01-27)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS
```

R CMD CHECK completed with `Status: OK`.


##### devel
```
R Under development (unstable) (2020-07-02 r78768)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS
```

R CMD CHECK completed with:

```
Status: 1 NOTE

* checking package dependencies ... NOTE
Imports includes 31 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally.
```



### Windows 


#####  release
```
R version 4.0.2 (2020-06-22) 
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18362)
```

R CMD CHECK completed with:

```
-- R CMD check results -------------------------------- dragon 1.0.0 ----
Duration: 7m 9.1s

> checking package dependencies ... NOTE
  Imports includes 31 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

0 errors √ | 0 warnings √ | 1 note x
```

#####  oldrelease
```
R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18362)
```

R CMD CHECK completed with:

```
-- R CMD check results -------------------------------- dragon 1.0.0 ----
Duration: 6m 34s

0 errors √ | 0 warnings √ | 0 notes √
```

#### devel
```
R Under development (unstable) (2020-06-29 r78751)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18362)
```

R CMD CHECK completed with:

```
CMD check results -------------------------------- dragon 1.0.0 ----
Duration: 6m 2.8s

> checking package dependencies ... NOTE
  Imports includes 31 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

0 errors √ | 0 warnings √ | 1 note x
```
