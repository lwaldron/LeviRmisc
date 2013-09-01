#!/bin/sh

cd ..
R --vanilla <<RSCRIPT
library(inlinedocs);
package.skeleton.dx("LeviRmisc", excludePattern="^heatmap")
RSCRIPT

R CMD build LeviRmisc

