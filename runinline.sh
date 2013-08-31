#!/bin/sh

cd ..
R --vanilla <<RSCRIPT
library(inlinedocs);
package.skeleton.dx("Rmisc", excludePattern="^heatmap")
RSCRIPT

R CMD build Rmisc

