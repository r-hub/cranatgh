# cranatgh

> 'CRAN' mirror at 'GitHub'

[![Linux Build Status](https://travis-ci.org/metacran/cranatgh.svg?branch=master)](https://travis-ci.org/metacran/cranatgh)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/metacran/cranatgh?svg=true)](https://ci.appveyor.com/project/gaborcsardi/cranatgh)
[![](http://www.r-pkg.org/badges/version/cranatgh)](http://www.r-pkg.org/pkg/cranatgh)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/cranatgh)](http://www.r-pkg.org/pkg/cranatgh)
[![Coverage Status](https://img.shields.io/codecov/c/github/metacran/cranatgh/master.svg)](https://codecov.io/github/metacran/cranatgh?branch=master)

The machinery behind the 'CRAN' mirror at &lt;https://github.com/cran&gt;.

## Installation

```r
remotes::install_github("metacran/cranatgh")
```

## Usage

`cranatgh` is used in the [web app that triggers Jenkins updates for CRAN @ GitHub  ](https://github.com/metacran/cranatgh.app). These updates are [triggered every minute](https://github.com/metacran/cron/blob/master/minutely/update-crandb.r#L28).

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi), [R Consortium](https://www.r-consortium.org/).
