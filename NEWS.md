# BayesRE 0.1.0

## Documentation and examples

- Added lightweight, automatically tested toy examples for `compute_metrics()`
  and `plot_observed_vs_predicted()`.
- Added lightweight input examples to the Stan-dependent documentation.
- Kept complete Stan fitting, prediction, diagnostic, and response-surface
  workflows in `\\donttest{}` because model compilation and sampling can exceed
  the 5-second per-Rd-file example limit.
