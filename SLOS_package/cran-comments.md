# cran-comments.md for Package Submission

## Summary

This is the second submission of the **SLOS** package to CRAN. 
All mistakes have been fixed from the previous iteration.
Notably, both examples take around one minute each to run, as they involve loading the model from the internet. They have been wrapped with \donttest{} 

## CRAN Submission Checklist

- [x] I have read and agree to the CRAN policies.
- [x] The package does not contain any sensitive data or confidential information.
- [x] The package passes `R CMD check` with no errors or warnings.
- [x] I have ensured that the package compiles on all platforms (Linux, macOS, Windows) by checking it on CRAN's testing platform.
- [x] The package is properly documented (including `DESCRIPTION`, `NAMESPACE`, and all functions).
- [x] There are no major dependencies or non-CRAN packages included in the package.

## Special Notes

- The package includes a machine learning model which is too large to be submitted to CRAN directly (over 1GB in size, size exceeds the 5 MB limit). To address this, the model is hosted externally on GitHub and can be downloaded dynamically using the function `load_SLOSModel()`. This ensures that the model is always up-to-date and avoids exceeding the CRAN size limit for package files.
  
- The URL for downloading the model is publicly accessible.

- I have tested the package on multiple systems and ensured that the downloading and loading of the model works without errors.

- There are no downstream dependencies.

## System Information

- R version: 4.4.1
- OS: Windows/macOS/Linux
- Package dependencies: `httr`, `caret`, `tidyverse`, `caretEnsemble`, `MLmetrics`, `ems`, `ranger`
