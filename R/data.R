#' Saha-Weeks (2022) candidate-choice conjoint
#'
#' A long-format forced-choice candidate-conjoint dataset from the
#' replication materials of Saha and Weeks (2022).  Respondents see
#' three forced-choice tasks, each showing a pair of hypothetical
#' political candidates varying on five attributes.  The goal of the
#' original study was to understand voter perceptions of women
#' candidates' ambition.
#'
#' This is the paper's full analysis frame: respondents with fewer than
#' three tasks or invalid demographics are dropped, and the moderator set
#' is the complete 19-covariate \eqn{\mathbf Z} used in
#' Acharya, Hainmueller, and Xu (2026).  Fitting it with the paper's
#' configuration reproduces the published candidate-choice results.
#'
#' @format A data frame with 7,146 rows (1,191 respondents x 3 tasks x
#'   2 profiles) and 37 columns:
#' \describe{
#'   \item{respondent, task, profile}{Identifiers (character / integer).}
#'   \item{choice}{Binary outcome (1 = this profile chosen, 0 = not).}
#'   \item{cand_genderMale, cand_runYes}{Candidate is male / has run for
#'     office before (0/1; references female / never run).}
#'   \item{cand_talentCollaborative, cand_talentDetermined.to.Succeed,
#'     cand_talentEmpathetic, cand_talentGood.Communicator,
#'     cand_talentHard.Working, cand_talentTough.Negotiator}{Talent
#'     dummies (reference = Assertive).}
#'   \item{cand_agendaModerate.Changes, cand_agendaComplete.Overhaul}{Policy
#'     agenda dummies (reference = Very Few Changes).}
#'   \item{cand_child1.child, cand_child2.children,
#'     cand_child3.children}{Number of children (reference = none).}
#'   \item{gender_num, age, income, educ_Middle, educ_High,
#'     party_Republican, party_Independent, region_NORTHEAST,
#'     region_SOUTH, region_WEST, employ_parttime, employ_homemaker,
#'     employ_not_working, employ_retired, employ_student,
#'     ideo_conservative, vote_trump, vote_clinton, gender_att}{The
#'     paper's full respondent covariate set \eqn{\mathbf Z} (19
#'     moderators): gender, age, income, education, party, region,
#'     employment status, ideology, vote choice, and a gender-attitudes
#'     scale.}
#'   \item{resp_party}{Convenience factor (Democrat / Independent /
#'     Republican) for subgroup labelling; not part of \eqn{\mathbf Z}.}
#' }
#' @source Saha, Sparsha and Jessica L. P. Weeks. 2022. "Ambitious
#'   Women: Gender and Voter Perceptions of Candidate Ambition."
#'   *Political Behavior* 44(2):779-805.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/KVTPVX} (CC0 1.0 public domain dedication).
#' @examples
#' data(sw2022)
#' head(sw2022)
"sw2022"

#' Graham-Svolik (2020) candidate-choice conjoint on democratic norms
#'
#' A long-format forced-choice candidate-conjoint dataset from the
#' replication materials of Graham and Svolik (2020).  Respondents see
#' up to 13 matchups, each showing a pair of hypothetical candidates
#' described by party, democracy behavior, policy positions, sex, race,
#' and profession.  The original study measures the extent to which U.S.
#' voters tolerate democratic backsliding by co-partisan candidates.
#'
#' The data is cleaned from the original experiment file: respondents
#' with incomplete matchups or missing moderator variables are dropped.
#'
#' This is the paper's full analysis frame in its two-profile differenced
#' parameterization; fitting the held-out 16-covariate specification with
#' the paper's configuration reproduces the published democratic-norms
#' results.
#'
#' @format A data frame with 41,314 rows (1,605 respondents, ~20,657
#'   matchups x 2 profiles) and 59 columns:
#' \describe{
#'   \item{respondent, task, profile}{Identifiers.}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{diff_respParty, diff_p1_num, diff_p2_num,
#'     diff_dem_code_g_committee, diff_dem_code_g_officestructure,
#'     diff_dem_code_g_procedure, diff_dem_code_g_progEval,
#'     diff_dem_code_g_record, diff_dem_code_g_schedule,
#'     diff_dem_code_u_banProtest, diff_dem_code_u_court,
#'     diff_dem_code_u_execRule, diff_dem_code_u_gerry2,
#'     diff_dem_code_u_gerry10, diff_dem_code_u_journalists,
#'     diff_dem_code_u_limitVote, diff_dem_code_v_affair,
#'     diff_dem_code_v_tax, diff_sex_Female, diff_race_Asian,
#'     diff_race_Black, diff_race_Hispanic, diff_pro_Farmer,
#'     diff_pro_Lawyer, diff_pro_Legislative_staffer,
#'     diff_pro_Police_officer, diff_pro_Served_in_the_army,
#'     diff_pro_Served_in_the_navy, diff_pro_Small_business_owner,
#'     diff_pro_Teacher}{Attribute contrasts differenced across the two
#'     profiles: co-partisan, economic / social policy (1-4 conservative),
#'     six good-governance codes (`g_*`), seven undemocratic codes
#'     (`u_*`), two valence codes (`v_*`), candidate sex, race (3), and
#'     profession (8).  Reference levels (good governance vs. "elect
#'     oversight board", etc.) are absorbed by the differencing, so no
#'     re-leveling is needed.}
#'   \item{z_ideo, z_pid7, z_trump, z_age, z_educ, z_hhi, z_auth,
#'     z_knowl, z_female, z_race_black, z_race_asian, z_race_other,
#'     E_ideal, I_ideal, M_ideal, T_ideal, dem_better, dem_satisfied,
#'     dem_treat_journalists, dem_treat_banProtest, dem_treat_execRule,
#'     dem_treat_ignoreCourt}{The paper's respondent covariate set
#'     \eqn{\mathbf Z} (22): ideology, party, Trump approval,
#'     demographics, authoritarianism, knowledge, and race; the four
#'     issue ideal points (`*_ideal`); and six direct democracy-attitude
#'     items (`dem_*`).  The paper's main specification holds out the six
#'     direct items and fits on the remaining 16.}
#'   \item{ideo7, pid7, weight}{Convenience columns (raw 1-7 ideology, raw
#'     -3..3 party ID, survey weight) for subgroup and weighting analyses;
#'     not part of \eqn{\mathbf Z}.}
#' }
#' @source Graham, Matthew H. and Milan W. Svolik. 2020. "Democracy
#'   in America?  Partisanship, Polarization, and the Robustness of
#'   Support for Democracy in the United States." *American Political
#'   Science Review* 114(2):392-409.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/EEARKA} (CC BY 4.0).
#' @examples
#' data(gs2020)
#' head(gs2020)
"gs2020"

#' Bechtel-Scheve (2013) climate-treaty conjoint
#'
#' A long-format forced-choice climate-treaty conjoint dataset from
#' the replication materials of Bechtel and Scheve (2013), US
#' subsample.  Respondents see four tasks, each showing a pair of
#' hypothetical climate agreements varying on cost, distribution,
#' participation, emissions, sanctions, and monitoring.  The numeric
#' cost attribute enables dollar-scale willingness-to-pay (WTP)
#' analysis via `sc_wtp()`.
#'
#' The data is filtered to US respondents (country == 4) with exactly
#' 8 rows (4 tasks x 2 profiles).
#'
#' @format A data frame with 20,000 rows (2,500 respondents x 4 tasks x
#'   2 profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (character).}
#'   \item{task}{Task id within respondent (1..4).}
#'   \item{profile}{Profile id within task (1 or 2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{cost_usd}{Numeric: monthly cost per household in USD
#'     (28, 56, 84, 113, or 141).  Supports dollar-scale WTP via
#'     `sc_wtp(fit, cost = "cost_usd", ...)`.}
#'   \item{distribution}{Factor: burden-sharing principle
#'     (ref: `Only rich pay`).}
#'   \item{participation}{Factor: number of participating countries
#'     (ref: `20 countries`).}
#'   \item{emissions}{Factor: emissions reduction target
#'     (ref: `40% reduction`).}
#'   \item{sanctions}{Factor: sanctions for non-compliance
#'     (ref: `No sanctions`).}
#'   \item{monitoring}{Factor: monitoring body
#'     (ref: `Your government`).}
#'   \item{resp_female}{Numeric 0/1: respondent gender (1 = female).}
#'   \item{resp_age}{Numeric: respondent age in years.}
#'   \item{resp_ideo}{Numeric 0-10: respondent ideology.}
#' }
#' @source Bechtel, Michael M. and Kenneth F. Scheve. 2013. "Mass
#'   Support for Global Climate Agreements Depends on Institutional
#'   Design." *Proceedings of the National Academy of Sciences*
#'   110(34):13763-13768.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/UGZ2BY} (CC0 1.0).
#' @examples
#' data(bs2013)
#' head(bs2013)
"bs2013"

#' Ballard-Rosa, Martin & Scheve (2017) tax-plan conjoint
#'
#' A long-format forced-choice tax-plan conjoint dataset from the
#' replication materials of Ballard-Rosa, Martin, and Scheve (2017).
#' Respondents see eight tasks comparing two tax-plan proposals.
#' Each plan specifies marginal tax rates for six income brackets
#' plus a revenue impact indicator. All attributes are numeric
#' (continuous), unlike the other bundled datasets which use factors.
#'
#' The sample is restricted to respondents who saw the revenue
#' column (saw_revenue == 1).
#'
#' All six bracket rates are rebuilt from the source file's coded
#' variables and their value labels rather than its derived rate
#' columns: the distributed file stores the 45 percent level of the
#' $175-375k bracket as the number 5 in its derived column, which
#' affected 19.9 percent of rows in copies of this dataset bundled
#' before version 0.2.0.9004.
#'
#' This is the paper's full analysis frame; fitting it with the paper's
#' `varref` recipe (`varref_floor = 1e-3`) reproduces the published
#' tax-preference results.
#'
#' @format A data frame with 32,000 rows (2,000 respondents x 8 tasks x
#'   2 profiles) and 35 columns:
#' \describe{
#'   \item{respondent, task, profile}{Identifiers.}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{rate_L10, rate_10_35, rate_35_85, rate_85_175, rate_175_375,
#'     rate_375P}{Marginal tax rate (percent) for the six income brackets.}
#'   \item{revenue_score}{Revenue impact: -2 (much less) to +2 (much more).}
#'   \item{resp_pid7}{Convenience column: raw seven-point party ID
#'     (1 = strong Democrat to 7 = strong Republican) for the by-party
#'     analyses; not part of \eqn{\mathbf Z} (its standardized counterpart
#'     `pid7_std` is).}
#'   \item{age_std, female, pid7_std, educ_std, race_white, income_std,
#'     ineq_averse, work_vs_luck, taxes_harm_econ, hardwork,
#'     high_econ_know, employed_ft, conserv_ideo, govt_serv, newsint,
#'     numeracy, gen_mobile, future_mobile, gov_assist, risk_averse,
#'     hardship, children, trust}{The paper's respondent covariate set
#'     \eqn{\mathbf Z} (23): demographics, partisanship, ideology, and
#'     economic attitudes and beliefs.  Continuous covariates are
#'     standardized and missing values median-imputed, matching the
#'     paper's prep.}
#' }
#' @source Ballard-Rosa, Cameron, Lucy Martin, and Kenneth Scheve. 2017.
#'   "The Structure of American Income Tax Policy Preferences."
#'   *Journal of Politics* 79(1):1-16.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/NGRGS5} (CC0 1.0).
#' @examples
#' data(br2017)
#' head(br2017)
"br2017"

#' Simulated conjoint with known ground truth
#'
#' A synthetic forced-choice conjoint dataset with known true
#' preference parameters, designed for validating the estimator.
#' The data-generating process uses three binary attributes and
#' two continuous respondent moderators:
#'
#' \deqn{\beta_1(Z_i) = 0.5 + 0.3 z_{1i}}
#' \deqn{\beta_2(Z_i) = -0.8 + 0.2 z_{2i}}
#' \deqn{\beta_3(Z_i) = 0.3}
#'
#' Attributes \eqn{x_1} and \eqn{x_2} are heterogeneous (preference
#' depends on respondent covariates); \eqn{x_3} is homogeneous.
#'
#' The true per-respondent \eqn{\beta} matrix is stored as
#' \code{attr(simdata, "beta_true")} and the DGP specification as
#' \code{attr(simdata, "dgp")}.
#'
#' @format A data frame with 12,000 rows (1,000 respondents x 6 tasks x 2
#'   profiles) and the following columns:
#' \describe{
#'   \item{respondent}{Respondent id (integer 1..1000).}
#'   \item{task}{Task id within respondent (1..6).}
#'   \item{profile}{Profile id within task (1..2).}
#'   \item{choice}{Binary outcome (1 = chosen, 0 = not chosen).}
#'   \item{x1}{Binary attribute 1.}
#'   \item{x2}{Binary attribute 2.}
#'   \item{x3}{Binary attribute 3.}
#'   \item{z1}{Continuous respondent moderator 1 (standard normal).}
#'   \item{z2}{Continuous respondent moderator 2 (standard normal).}
#' }
#' @examples
#' data(simdata)
#' dim(simdata)
#' head(simdata)
#' ## True beta matrix:
#' beta_true <- attr(simdata, "beta_true")
#' dim(beta_true)
"simdata"
