# Nowcasting nomenclature

Throughout this package, we will refer to different nowcasting object
lists, matrices, and vectors using the following table. In this table,
“point” refers to a point estimate. When not indicated, we are referring
to a probabilistic draw from an observation model.

[TABLE]

For example, we refer to the matrix with imputed point estimates for all
\\t+d\>t^\*\\ as a point nowcast matrix, a matrix with a complete set of
observations for all elements as a reporting matrix, and a matrix with
only the predictions as a point prediction matrix.

In package documentation, we will generally use a reporting triangle to
refer to a diagonal reporting triangle, which is the special case of a
reporting triangle where the reporting delays and reference times are
indexed on the same scale. An example of a diagonal reporting triangle
is one with daily reporting of daily data. The package also supports
what we refer to as a ragged reporting triangle, an example of which
would be daily reporting of data referenced only once per week.

We will use the following to abbreviations to shorten names in the code
when necessary:

| **Long Name** | **Code Abbreviation** |
|---------------|-----------------------|
| truncated     | `trunc`               |
| retrospective | `retro`               |
| observed      | `obs`                 |
| predicted     | `pred`                |
| matrix        | `mat`                 |
| point         | `pt`                  |
| error         | `err`                 |
