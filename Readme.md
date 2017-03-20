#sicegar package

Package for single-cell virology.

This package aims to quantify time intensity data by using sigmoidal and double sigmoidal curves. It fits straight lines, sigmoidal, and double sigmoidal curves on to time vs intensity data. Then all the fits are used to make decision on which model (sigmoidal, double sigmoidal, no signal or ambiguous) best describes the data. No signal means the intensity does not reach a high enough point or does not change at all over time. Sigmoidal means intensity starts from a small number than climbs to a maximum. Double sigmoidal means intensity starts from a small number, climbs to a maximum then starts to decay. After the decision between those four options, the algorithm gives the sigmoidal (or double sigmoidal) associated parameter values that quantifies the time intensity curve. The origin of the package name came from "SIngle CEll Growth Analysis in R".
URL: https://github.com/wilkelab/sicegar

