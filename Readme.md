#sicegar package

Package for single-cell virology.

the package aims to quantify time intensity data by using sigmoidal and double sigmoidal curves. It fits straight lines, sigmoidal and double sigmoidal curves on to time vs intensity data. The all the fits together used to make decision between sigmoidal, double sigmoidal, no signal or ambiguous. No signal means the intensity do not reach to a high enough point or do not change at all. Sigmoidal means intensity start from a small number than climb to a maximum. Double sigmoidal means intensity start from a small number, climb to a maximum then starts to decay. After the decision between those four options, algorithm gives sigmoidal (or double sigmoidal) associated parameter values that quantifies the time intensity curve.
