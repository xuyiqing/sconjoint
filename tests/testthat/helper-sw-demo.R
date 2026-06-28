## Pre-0.2.1 friendly `sw2022` layout (factor attributes + 3 raw moderators),
## reconstructed from the full bundled data. Feature tests that predate the
## 0.2.1 full-data switch fit on `sw2022_demo` so their formulas keep working.
sw2022_demo <- local({
  e <- environment()
  utils::data(sw2022, package = "sconjoint", envir = e)
  d <- e$sw2022
  mk <- function(dummies, labs, ref) {
    v <- rep(ref, nrow(d))
    for (i in seq_along(dummies)) v[d[[dummies[i]]] == 1] <- labs[i]
    factor(v, levels = c(ref, labs))
  }
  d$cand_gender  <- factor(ifelse(d$cand_genderMale == 1, "Male", "Female"),
                           levels = c("Male", "Female"))
  d$prior_office <- factor(ifelse(d$cand_runYes == 1, "Yes", "No"),
                           levels = c("No", "Yes"))
  d$agenda <- mk(c("cand_agendaModerate.Changes", "cand_agendaComplete.Overhaul"),
                 c("Moderate Changes", "Complete Overhaul"), "Very Few Changes")
  d$talent <- mk(c("cand_talentCollaborative", "cand_talentDetermined.to.Succeed",
                   "cand_talentEmpathetic", "cand_talentGood.Communicator",
                   "cand_talentHard.Working", "cand_talentTough.Negotiator"),
                 c("Collaborative", "Determined to Succeed", "Empathetic",
                   "Good Communicator", "Hard-Working", "Tough Negotiator"),
                 "Assertive")
  d$children <- mk(c("cand_child1.child", "cand_child2.children", "cand_child3.children"),
                   c("1 child", "2 children", "3 children"), "No children")
  d$resp_female <- d$gender_num
  d$pid <- d$resp_party
  d[, c("respondent", "task", "profile", "choice", "agenda", "talent",
        "children", "cand_gender", "prior_office", "resp_female", "age", "pid")]
})
