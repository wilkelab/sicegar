# R package sicegar

[![Build Status](https://travis-ci.org/wilkelab/sicegar.svg?branch=master)](https://travis-ci.org/wilkelab/sicegar)
[![Coverage Status](https://img.shields.io/codecov/c/github/wilkelab/sicegar/master.svg)](https://codecov.io/github/wilkelab/sicegar?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sicegar)](https://CRAN.R-project.org/package=sicegar)
[![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/sicegar?color=brightgreen)](http://cranlogs.r-pkg.org/downloads/total/last-month/sicegar)


Written by M. Umut Caglar and Claus O. Wilke.

This package aims to quantify time intensity data by using sigmoidal and double sigmoidal curves. It fits sigmoidal and double sigmoidal curves on to time vs intensity data. Then both fits are used to make a decision on which model (sigmoidal, double sigmoidal, no signal, or ambiguous) best describes the data. No signal means the intensity does not reach a high enough point or does not change at all over time. Sigmoidal means intensity starts from a small number than climbs to a maximum. Double sigmoidal means intensity starts from a small number, climbs to a maximum then starts to decay. Once the decision has been reached, the algorithm returns the parameters associated with the sigmoidal or double-sigmoidal model quantifying the time-intensity curve.

The package name stands for "SIngle CEll Growth Analysis in R".

CRAN: https://cran.r-project.org/package=sicegar

Source: https://github.com/wilkelab/sicegar



