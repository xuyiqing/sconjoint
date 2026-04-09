# data-raw/

Scripts that build the small bundled datasets under `data/*.rda` from
their full sources. Each script downsamples a published conjoint
dataset (Saha-Weeks 2022, Graham-Svolik 2020, Bechtel-Scheve 2013) to
under 500 KB so the package data footprint stays small.

This directory is excluded from the package build via `.Rbuildignore`.
It will be populated in a later milestone (M5).
