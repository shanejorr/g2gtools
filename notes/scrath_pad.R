# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)
library(googlesheets4)
library(g2gtools)

# devtools::load_all()

# install fonts
library(showtext)

# show families
font_families()

# look up installed familes to add
font_files <- font_files()

# add family
sysfonts::font_add("Segoe UI", "segoeui.ttf")

# make sure it was added
font_families()

'# teacher survey --------------------------
