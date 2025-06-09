install.packages("IRkernel")
IRkernel::installspec(user = FALSE)

packages <- c(
  "caret",
  "caretEnsemble",
  "tidyverse",
  "MLmetrics",
  "ranger",
  "DescTools",
  "mice"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed) {
    install.packages(pkg, dependencies = TRUE)
  }
}
