library(sconjoint)
cat("quantities overview\n")
sc_subgroup(fit, subgroup = list(female = fit$Z[, "resp_female"] > 0.5,
                                 male   = fit$Z[, "resp_female"] < 0.5))
sc_compensating(fit, benefit = "benefit_attr:level",
                     cost    = "cost_attr:level")
sc_clusters(fit, k = 3L, seed = 1L)
