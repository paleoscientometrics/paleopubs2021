Publication pressure threatens the integrity of palaeontological research
================

*Nussaïbah B. Raja & Emma Dunne*

  - [Description](#description)
  - [Requirements](#requirements)
  - [Setup](#setup)
  - [Scripts](#scripts)
  - [Troubleshooting](#troubleshooting)

## Description

This repository contains all the **data** (in `/data`) and **R scripts**
(in `/scripts`) necessary to reproduce Figs. 1 and 2, and the draft figure of Fig 3 of the following preprint:

[Add citation to preprint when available]

The final figures in the `/figs` folder. 

## Requirements

This code was developed in `R 4.0.0`. It is therefore recommended to use the same or any more up-to-date version of R for reproducing the analyses in this study.

## Setup

You will need to either use the Rstudio project environment or set your
working directory to the root of this folder.

To install all required depdendencies (packages), run:

``` r
source(file.path("inst","dependencies"))
```

## Scripts

The `scripts/` folder contains all the code generated for the above mentioned study. The name of each of the scripts refers to the corresponding figure it produces in the publication.

## Troubleshooting

The issue tracker is the preferred channel for bug reports. You may also
contact me [by email](mailto:nussaibah.raja.schoob@fau.de).
