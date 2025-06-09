packages <- c(
  "caret",
  "caretEnsemble",
  "tidyverse",
  "MLmetrics",
  "ranger",
  "DescTools",
  "mice",
  "IRkernel"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed) {
    install.packages(pkg, dependencies = TRUE)
  }
}

IRkernel::installspec(user = FALSE)
