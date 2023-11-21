# This file represents scrath work. Don't assume anything in this file actually works.
# It is not included in the package build.
library(tidyverse)

devtools::load_all()

ppt_file <- "~/repos/tntp/g2gtools/inst/extdata/tntp_template_no_slides.pptx"

pptx_layout <- officer::read_pptx(ppt_file) |>
  officer::layout_properties()

# initialize Power Point
doc <- g2g_create_deck_ppt("This is the title")

doc <- g2g_add_text_slide(doc, "header title", "Slide text")

sentences <- c(
  "The quick brown fox jumps over the lazy dog.",
  "Tomorrow's weather promises sunshine and mild breezes.",
  "Reading a good book is a wonderful way to relax.",
  "Technology is rapidly changing the modern world."
)

df <- data.frame(
  y = c(10, 15, 7, 11),
  x = sentences
)

plt <- g2g_viz_basic_bar(df, 'x', 'y', 'y', text_offset = -.15, fill_color = 'gray', text_color = 'black', text_size = 4.21, font_face = "plain") +
  coord_flip()

doc <- g2g_add_viz_ppt(
  doc, plt, "This is the header",
  plt_width = 9,
  plt_height = 5, notes_text = 'Random notes', text_box = "Here is some random text", vector_plt = FALSE)

print(doc, 'a.pptx')
