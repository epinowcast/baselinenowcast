---
title: "Mathematical description of `baselinenowcast` method"
output: html_document
---

## `baselinenowcast` mathematical model

The following describes the estimate of the delay distribution, the generation
of the point nowcast, and the estimate of the observation error, for a
partially observed or complete reporting triangle.
The method assumes that the units of the delays, $d$$ and the units of
reference time $t$ are the same, e.g. weekly data and weekly releases or
daily data with daily releases.
This method is based on the method described by (Wolffram et al. 2023)
developed by the Karlsruhe Institute of Technology.

### Notation

We denote $X_{t,d}, d = 0, .., D$ as the number of cases occurring on time $t$
which appear in the dataset with a delay of $d$.
For example, a delay $d = 0$ means that a case occurring on day $t$ arrived
in the dataset on day $t$, or on what is considered to be the first possible
report date in practice.
We only consider cases reporting within a maximum delay $D$.
The number of cases reporting for time $t$ with a delay of at most $d$ can be
written as:

$$X_{t, \le d} = \sum_{i=0}^d X_{t,i} $$

Such that $X_t = X{t, \le D}$ is the “final” number of reported cases on time $t$. Conversely, for $d < D$

$$X_{t,>d} = \sum_{i = d+1} ^{D} X_{t,i}$$

is the number of cases still missing after $d$ delays.

We refer to $X_t$ to describe a random variable, $x_t$ for the corresponding
observation, and $\hat{x}_t$ for an estimated/imputed value.

We refer to the matrix of $x$ available at a given data release time $t^*$,
as the reporting triangle. Here, all $t+d > t^*$ have yet to be observed.

* Need to fix alignment so this looks like a table.
$$
\begin{align}
x_{1,1} \ x_{1,2} \ x_{1,3} \ ... \ x_{1, D-1} \ x_{1,D} \\
x_{2,1} \ x_{2,2} \ x_{2,3} \ ... \ x_{2, D-1} \ x_{2,D} \\
x_{3,1} \ x_{3,2} \ x_{3,3} \ ... \ x_{3, D-1} \ x_{3,D} \\
x_{t^*-1, 1} \ x_{t^*-1, 2} \ x_{t^*-1, 3} \ ... \ x_{t^*-1, D-1}, x_{t^*-1, D} \\
x_{t^*, 1} \ x_{t^*, 2} \ x_{t^*, 3} \ ... \ x_{t^*, D-1}, x_{t^*, D}
\end{align}
$$

### Point estimate of the delay distribution
We use the entire reporting triangle to compute an empirical estimate of the
delay distribution, $\rho(d)$, or the probability that a case at reference time
$t$ appears in the dataset at time $t + d$.
We will refer to the realized empirical estimate of the delay distribution from a reporting triangle as $\hat{\rho}(d)$.

The delay distribution, $\rho(d)$ can be estimated directly from the completed reporting matrix $X$
$$
{\rho}_d= \frac{\sum_{t=1}^{t=t^*} X_{t,d}}{\sum_{d=0}^{D} \sum_{t=1}^{t=t^*} X_{t,d}}
$$

In the special case when the time the estimate is made $t'$, is beyond the data
release time $t^{*}$, such that $t' \ge t^* + D$, $\hat{\rho}_d$ can be
computed directly by summing over all reference time points $t$  at each
delay $d$.

In the case where there are partial observations, in order to properly weight
the denominator with the missing delays, we have to first impute the cases
$\hat{x}_{t,d}$ for all instances where $t+d > t^*$.
This amounts to computing the point nowcast from the partial reporting triangle.

### Point nowcast from incomplete reporting matrix

To do so, we start by defining $\theta_d$, which is the factor by which the
cases on delay $d$ compare to the total cases through delay $d-1$, which can be
computed as:

$$
\hat{\theta}_d(t^*) = \frac{\sum_{t=1}^{t^*- d } x_{t, d}}{\sum_{d=1}^{d-1} \sum_{t=1}^{t^*-d} x_{t,d}}
$$
*Note this amounts to taking the sum of the elements in column $d$ up until time
$t*-d$ and dividing by the sum over all the elements to the left of column $d$
up until time $t*-d$, referred to as `block_top` and `block_top_left` in the
code *

This factor is then used to compute the expected values for all the missing
rows with delay $d$ as:
$$
\hat{x}_{t,d} = \hat{\theta_d}(t^*) \times \sum_{t=t^*-d}^{t^*} x_{t,d-1}
$$
* Note this amounts to effectively taking the sum across the columns in the
bottom left (`block_bottom_left` in the code) of the matrix up until column
$d$ and multiplying by the factor to estimate the value of $x_{t,d}$ in
each row. *

This process is repeated iteratively, from left to right, to impute the bottom
right triangle of the reporting matrix for each time $t$ and delay $d$ when
$t+d>t^*$.
The combination of the imputed and observed reporting matrix is then used to
compute the delay distribution $\hat{\rho}(d)$ as described above.

The factor, $\theta_d$ can only be computed for delays greater than 0, which
effectively means that a row of the reporting triangle without any
observations, can not be imputed as the process works iteratively.
Therefore, the reporting matrix must have at least an entry (though it can be
0) for $x_{t*,d=0}$ to compute a nowcast for $t^*$.
We will describe the method for estimating nowcasts for zero-valued counts
below.

We can then use the delay distribution $\hat{\rho(d)}$ to compute directly
a point nowcast, without the iterative imputation to complete the reporting
triangle.


### Point nowcast from delay distribution $\rho(d)$
The point nowcast of an incomplete reporting triangle can be computed directly
from the delay distribution, $\rho(d)$.

We start by computing the expected total number of eventual observed cases
$\hat{x}_t$, for each reference time $t$, by summing over all delays $d$ that
have already been observed (up until $t*-t$) and dividing by the cumulative sum
of the delay distribution

$$
\hat{x}_t= \frac{\sum_{d=1}^{d=t*-t} x_{t,d}}{\sum_{d=1}^{d=t*-t} \hat{\rho}(d)}
$$

Then we can compute $\hat{x}_t,d$ directly using the $d$th element of
$\hat{\rho}(d)$

$$
\hat{x}_{t,d} = \hat{\rho}(d) \times \hat{x}_t

$$
Here we multiply the expected total by the proportion expected at that
particular delay $d$.

### Estimate of uncertainty in the nowcast

*Filling in tomorrow*
