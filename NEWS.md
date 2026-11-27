# sconjoint 0.4.1

* The package now fits one model, the structural random-utility model of Acharya, Hainmueller and
  Xu (2026): a mixed logit for repeated choices whose mean tastes come from a neural network, with
  `q` taste factors.
* Workflow: `sc_design()`, `sc_code()`, `sc_data()`, `sc_example_data()`, `sc_fit()`,
  `sc_select_rank()`, `sc_states()`, `sc_save()`, `sc_load()`.
* Quantities: `sc_mean()`, `sc_share()`, `sc_contest()`, `sc_ame_switch()`, `sc_importance()`,
  `sc_decisiveness()` and others, estimated by `sc_estimate()` with a cross-fitted one-step
  correction; `sc_posterior()` for individual respondents.
* Plots and tables in base graphics: `sc_plot_estimates()`, `sc_plot_laws()`,
  `sc_plot_preferences()`, `sc_plot_importance()`, `sc_plot_lines()`, `sc_plot_posterior()`,
  `sc_table()`.
* Data: `gs2020` and `br2017` as used in the paper; `simconjoint`, a simulated experiment with a
  known truth.
* Removed: `scfit()`, `scmix()` and their functions, the datasets `sw2022`, `bs2013` and
  `simdata`, and the dependence on 'torch'.

# sconjoint 0.2.2

* Earlier estimator `scfit()` (a deep network trained with 'torch') with debiased quantities and
  five bundled datasets; fixes from the CRAN review of 0.2.1.
