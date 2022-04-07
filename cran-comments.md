## Patch version update
This submission is a patch version update to 1.2.1 of the package `dragon`, currently on CRAN as version 1.2.0.


All R CMD checks are passing on:

+ Local MacOS (R version 4.1.2). Note that MacOS release also builds via GitHub Actions.
+ Linux Ubuntu release and devel via Github Actions
+ Win-builder oldrelease, release, devel. Note that Windows release also builds via GitHub Actions.

The local MacOS build has the following `NOTE`:

```
* checking package dependencies ... NOTE
Imports includes 32 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally.
```

All Win-builder builds run on `winbuilder` have the following `NOTE`:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Stephanie J. Spielman <spielman@rowan.edu>'
```



All OS builds in GitHub Actions R CMD CHECK have the `NOTE`:

```
❯ checking package dependencies ... NOTE
  Imports includes 32 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```