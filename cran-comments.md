## Patch version update
This submission is a patch version update to 1.2.1 of the package `dragon`, currently on CRAN as version 1.2.0. This update ensures compatibility with a newer `igraph` dependency version and updates the maintainer email.

In addition, it has come to my attention that my previous email had been the source of spam to CRAN maintainers. My absolute sincerest apologies. I unfortunately have no control over the behavior of this former email, and I have reached out to the IT team who now controls it to tell them to stop spamming organizations.


All R CMD checks are passing on:

+ Local MacOS (R version 4.1.2). Note that MacOS release also builds via GitHub Actions.
+ Linux Ubuntu release and devel via Github Actions
+ Win-builder oldrelease and devel but not release. Note that Windows release also builds via GitHub Actions.


The Win-builder `release` fails with this error that I believe is because the Win-builder release instance doesn't yet contain igraph version 1.3.0, which went to CRAN on 4/1/2022.

```
Package required and available but unsuitable version: 'igraph'
```

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
Maintainer: 'Stephanie J. Spielman <stephanie.spielman@gmail.com>'
```



All OS builds in GitHub Actions R CMD CHECK have the `NOTE`:

```
❯ checking package dependencies ... NOTE
  Imports includes 32 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```