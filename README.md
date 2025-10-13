
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Baseline nowcast <a href="https://baselinenowcast.epinowcast.org/"><img src="man/figures/logo.png" align="right" height="139" alt="baselinenowcast website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-yellow.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/epinowcast/baselinenowcast/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/epinowcast/baselinenowcast/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epinowcast/baselinenowcast/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epinowcast/baselinenowcast)
[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/epinowcast/baselinenowcast/blob/master/LICENSE.md/)
[![GitHub
contributors](https://img.shields.io/github/contributors/epinowcast/baselinenowcast)](https://github.com/epinowcast/baselinenowcast/graphs/contributors)
<!-- badges: end -->

<!-- badges to add eventually: [![Universe](https://epinowcast.r-universe.dev/badges/baselinenowcast)](https://epinowcast.r-universe.dev/baselinenowcast) -->

## Summary

Nowcasting right-truncated epidemiological data is critical for timely
public health decision-making, as reporting delays can create misleading
impressions of declining trends in recent data. This package provides
simple nowcasting methods for practical use based on using empirical
delay distributions and uncertainty from past performance. It is also
designed to be used as a baseline method for developers of new
nowcasting methods. It supports standard data frame inputs with
reference date, report date, and count columns, is compatible with
‘epinowcast’ objects, and also supports direct use of reporting
triangles. Alongside an opinionated default workflow, it has a low-level
pipe-friendly modular interface, allowing context-specific workflows. It
can accommodate a wide spectrum of reporting schedules, including mixed
patterns of reference and reporting (daily-weekly, weekly-daily). It
also supports sharing delay distributions and uncertainty estimates
between strata, as well as custom uncertainty models and delay
estimation methods.

## Installation

<details>
<summary>
Installing the package
</summary>

To install the development version from GitHub (warning! this version
may contain breaking changes and/or bugs), use the [`pak`
package](https://pak.r-lib.org/):

``` r
pak::pak(file.path("epinowcast", "baselinenowcast"))
```

</details>

## Resources

We provide a range of other documentation, case studies, and community
spaces to ask (and answer!) questions:

<details>
<summary>
Organisation Website
</summary>

Our [organisation website](https://www.epinowcast.org/) includes links
to other resources, [guest posts](https://www.epinowcast.org/blog.html),
and [seminar schedule](https://www.epinowcast.org/seminars.html) for
both upcoming and past recordings.

</details>
<details>
<summary>
Community Forum
</summary>

Our [community forum](https://community.epinowcast.org/) has areas for
[question and answer](https://community.epinowcast.org/c/interface/15)
and [considering new methods and
tools](https://community.epinowcast.org/c/projects/11), among others. If
you are generally interested in real-time analysis of infectious
disease, you may find this useful even if you do not use
`baselinenowcast`.

</details>

## Contributing

We welcome contributions and new contributors! We particularly
appreciate help on [identifying and identified
issues](https://github.com/epinowcast/baselinenowcast/issues). Please
check and add to the issues, and/or add a [pull
request](https://github.com/epinowcast/baselinenowcast/pulls) and see
our [contributing
guide](https://github.com/epinowcast/.github/blob/main/CONTRIBUTING.md)
for more information.

### How to make a bug report or feature request

Please briefly describe your problem and what output you expect in an
[issue](https://github.com/epinowcast/baselinenowcast/issues). See our
[contributing
guide](https://github.com/epinowcast/.github/blob/main/CONTRIBUTING.md)
for more information.

### Code of Conduct

Please note that the `baselinenowcast` project is released with a
[Contributor Code of
Conduct](https://github.com/epinowcast/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citation

If you use `baselinenowcast` in your work, please consider citing it
with `citation("baselinenowcast")`.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<a href="https://github.com/epinowcast/baselinenowcast/commits?author=kaitejohnson">kaitejohnson</a>,
<a href="https://github.com/epinowcast/baselinenowcast/commits?author=seabbs">seabbs</a>,
<a href="https://github.com/epinowcast/baselinenowcast/commits?author=TimTaylor">TimTaylor</a>,
<a href="https://github.com/epinowcast/baselinenowcast/commits?author=sbfnk">sbfnk</a>

### Issue Authors

<a href="https://github.com/epinowcast/baselinenowcast/issues?q=is%3Aissue+author%3Ajonathonmellor">jonathonmellor</a>,
<a href="https://github.com/epinowcast/baselinenowcast/issues?q=is%3Aissue+author%3Aswo">swo</a>

### Issue Contributors

<a href="https://github.com/epinowcast/baselinenowcast/issues?q=is%3Aissue+commenter%3Ajamesmbaazam">jamesmbaazam</a>,
<a href="https://github.com/epinowcast/baselinenowcast/issues?q=is%3Aissue+commenter%3Ajbracher">jbracher</a>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
<!-- will add this in later -->
