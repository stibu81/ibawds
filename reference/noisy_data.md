# Noisy Data From a Tenth Order Polynomial

Training and test data created from a tenth order polynomial with added
noise. The polynomial is given by \$\$f(x) = 2 x - 10 x^5 + 15
x^{10}\$\$ The noise follows a standard normal distribution. The data
can be used to demonstrate overfitting. It is inspired by section II. B.
in [A high-bias, low-variance introduction to Machine Learning for
physicists](https://arxiv.org/abs/1803.08823)

## Usage

``` r
noisy_data
```

## Format

a list of two tibbles with two columns each. \\x\\ stands for the
independent, \\y\\ for the dependent variable. The training data
(`noisy_data$train`) contains 1000 rows, the test data
(`noisy_data$test`) 20 rows.

## References

P. Mehta et al., *A high-bias, low-variance introduction to Machine
Learning for physicists* Phys. Rep. 810 (2019), 1-124.
[arXiv:1803.08823](https://arxiv.org/abs/1803.08823)
[doi:10.1016/j.physrep.2019.03.001](https://doi.org/10.1016/j.physrep.2019.03.001)
