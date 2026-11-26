#' Graham and Svolik (2020) candidate-choice conjoint on democratic norms
#'
#' A long-format forced-choice candidate conjoint from the replication
#' materials of Graham and Svolik (2020). Respondents see up to 13 matchups.
#' Each matchup shows two hypothetical candidates described by party,
#' democracy behavior, economic and social policy positions, sex, race and
#' profession. The original study asks how far U.S. voters tolerate
#' undemocratic behavior by candidates of their own party.
#'
#' The sample is the paper's analysis frame: 1,605 respondents with complete
#' base covariates and 20,657 matchups with an observed choice. Each matchup
#' takes two rows. Profile 1 holds the 30 attribute contrasts (candidate 1
#' minus candidate 2) and `choice` = 1 if candidate 1 won. Profile 2 is a row
#' of zeros with the opposite choice, so the contrast of the two rows is the
#' profile 1 row itself.
#'
#' Candidate 1 is not always the one shown on the left of the screen: in 10,275
#' of the 20,657 matchups it was shown on the right. Because the model's
#' position effect is defined by screen side, the paper's fits put the
#' left-displayed candidate first: y = 1 means the left candidate was chosen
#' and d = left minus right. Use `on_left` to rebuild that orientation, or call
#' [sc_example_data()], which does it for you.
#'
#' @format A data frame with 41,314 rows (20,657 matchups x 2 profiles) and 60
#'   columns:
#' \describe{
#'   \item{respondent, task, profile}{Identifiers. `task` numbers a
#'     respondent's matchups 1, 2, ... in file order.}
#'   \item{choice}{1 if this profile was chosen, 0 if not.}
#'   \item{on_left}{1 if this row's candidate was displayed on the left of
#'     the screen, 0 if on the right (raw `c_onLeft` on profile 1 rows,
#'     `o_onLeft` on profile 2 rows). The two rows of a matchup always sum
#'     to 1.}
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
#'     diff_pro_Teacher}{Attribute contrasts, candidate 1 minus candidate 2:
#'     co-partisan, economic and social policy (1-4, conservative high), six
#'     good-governance codes (`g_*`, reference "elect oversight board"),
#'     seven undemocratic codes (`u_*`), two valence codes (`v_*`), candidate
#'     sex, race (3) and profession (8).}
#'   \item{z_ideo, z_pid7, z_trump, z_age, z_educ, z_hhi, z_auth, z_knowl,
#'     z_female, z_race_black, z_race_asian, z_race_other, E_ideal, I_ideal,
#'     M_ideal, T_ideal}{The 16 respondent covariates of the paper's main fit:
#'     ideology, party, Trump approval, age, education, income,
#'     authoritarianism, political knowledge (standardized), sex and race
#'     (0/1), and four issue ideal points (standardized).}
#'   \item{dem_better, dem_satisfied, dem_treat_journalists,
#'     dem_treat_banProtest, dem_treat_execRule, dem_treat_ignoreCourt}{Six
#'     direct democracy-attitude items. The paper holds them out of the main
#'     fit.}
#'   \item{ideo7, pid7, weight}{Raw seven-point ideology (1 = extremely
#'     liberal), raw party ID (-3 = strong Democrat to 3 = strong Republican,
#'     0 = pure independent) and the survey weight. Used for subgroups, not
#'     as model inputs.}
#' }
#' @source Graham, Matthew H. and Milan W. Svolik. 2020. "Democracy
#'   in America? Partisanship, Polarization, and the Robustness of
#'   Support for Democracy in the United States." *American Political
#'   Science Review* 114(2):392-409.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/EEARKA} (CC BY 4.0).
#' @seealso [sc_example_data()] for the paper's model inputs built from these
#'   data.
#' @examples
#' data(gs2020)
#' head(gs2020[, 1:8])
#' # Share of matchups whose candidate 1 was shown on the right
#' mean(gs2020$on_left[gs2020$profile == 1] == 0)
"gs2020"

#' Ballard-Rosa, Martin and Scheve (2017) tax-plan conjoint
#'
#' A long-format forced-choice tax-plan conjoint from the replication
#' materials of Ballard-Rosa, Martin and Scheve (2017). Respondents see eight
#' tasks. Each task compares two tax plans (Plan A is profile 1). A plan sets
#' the marginal tax rate in six income brackets and shows how much revenue it
#' raises compared with current policy.
#'
#' The sample is the 2,000 respondents who saw the revenue attribute. All six
#' bracket rates are rebuilt from the file's coded variables and their value
#' labels, because the file's derived column for the $175-375k bracket stores
#' the 45 percent rate as 5.
#'
#' The paper's models use 1,991 respondents: the 9 with no income answer are
#' dropped (`valid_income == FALSE`). Their network inputs are the raw codes
#' (age in years, education and party dummies, the income code), not the
#' standardized columns. [sc_example_data()] builds those inputs.
#'
#' @format A data frame with 32,000 rows (2,000 respondents x 8 tasks x 2
#'   plans) and 39 columns:
#' \describe{
#'   \item{respondent, task, profile}{Identifiers (profile 1 = Plan A).}
#'   \item{choice}{1 if this plan was chosen, 0 if not.}
#'   \item{rate_L10, rate_10_35, rate_35_85, rate_85_175, rate_175_375,
#'     rate_375P}{Marginal tax rate (percent) in the six brackets.}
#'   \item{revenue_score}{Revenue compared with current policy: -2 (much
#'     less) to 2 (much more). The raw code is `taxrev = 3 - revenue_score`
#'     (1 = much more, 5 = much less).}
#'   \item{resp_pid7}{Raw party ID: 1 = strong Democrat to 7 = strong
#'     Republican, 4 = independent, 8 = not sure.}
#'   \item{resp_age}{Age in years (raw).}
#'   \item{resp_educ}{Education code (raw): 1 = no high school, 2 = high
#'     school graduate, 3 = some college, 4 = two-year degree, 5 = four-year
#'     degree, 6 = post-graduate.}
#'   \item{resp_income}{Household income code (raw `hh_income`), 1 = under
#'     $10,000 to 13 = over $375,000; `NA` for the 9 respondents who skipped
#'     the question.}
#'   \item{valid_income}{`TRUE` if the respondent gave an income answer. The
#'     paper's sample is the 1,991 respondents with `TRUE`.}
#'   \item{age_std, female, pid7_std, educ_std, race_white, income_std,
#'     ineq_averse, work_vs_luck, taxes_harm_econ, hardwork,
#'     high_econ_know, employed_ft, conserv_ideo, govt_serv, newsint,
#'     numeracy, gen_mobile, future_mobile, gov_assist, risk_averse,
#'     hardship, children, trust}{A wider set of 23 respondent covariates:
#'     demographics, partisanship, ideology, and economic attitudes and
#'     beliefs. Continuous ones are standardized and missing values
#'     median-imputed (`income_std` gives the 9 respondents without an income
#'     answer the median). `female` and `race_white` are 0/1 and also enter
#'     the paper's raw inputs.}
#' }
#' @source Ballard-Rosa, Cameron, Lucy Martin, and Kenneth Scheve. 2017.
#'   "The Structure of American Income Tax Policy Preferences."
#'   *Journal of Politics* 79(1):1-16.
#'   Data from the published replication materials, Harvard Dataverse
#'   \doi{10.7910/DVN/NGRGS5} (CC0 1.0).
#' @seealso [sc_example_data()] for the paper's model inputs built from these
#'   data.
#' @examples
#' data(br2017)
#' head(br2017[, 1:11])
#' # Respondents without an income answer
#' unique(br2017$respondent[!br2017$valid_income])
"br2017"

#' A simulated candidate-choice conjoint with known truth
#'
#' A small simulated experiment for learning the package and for checking
#' that the model recovers what it should. Each of 800 respondents makes 10
#' choices between two hypothetical candidates. A candidate has four
#' attributes: party (Democrat, Republican), experience (none, some, long),
#' policy position (liberal, moderate, conservative) and gender (male,
#' female). Each attribute's level is drawn uniformly and independently, as
#' in [sc_design()]. Each respondent has three characteristics: age, a
#' seven-point ideology, and a 0/1 indicator for women.
#'
#' The choices follow the package's model exactly. Respondent `i` chooses the
#' first candidate of a task with probability `plogis(kappa + a_i + d' beta_i)`,
#' where `d` is the first candidate's coded attributes minus the second's
#' (treatment dummies against the first level of each attribute: six
#' features), `beta_i = mu(Z_i) + Lambda u_i` with two taste factors
#' (`u_i` standard normal in two dimensions), and `a_i = sqrt(omega) v_i`
#' is the respondent's persistent pull toward the first candidate. Here
#' `kappa = 0.2` and `omega = 0.3`.
#'
#' Mean tastes `mu(Z)` move smoothly, and not linearly, with the
#' respondent's characteristics. With `a = (age - 49) / 18` and
#' `s = ideology / 3`: the taste for a Republican is `1.5 tanh(2 s) + 0.1 a`
#' (it saturates for strong partisans); for some and for long experience
#' `0.4 + 0.15 a` and `0.6 + 0.35 tanh(1.5 a)`; for a moderate and for a
#' conservative position (against a liberal one) `g (1 + 2 s)` and `g 4 s`,
#' with `g = 0.25 + 0.25 s^2`, which is a quadratic loss around the
#' respondent's own position that weighs more for those at the extremes; and
#' for a woman candidate `0.15 + 0.35 female - 0.15 a`. The first taste factor
#' moves party and policy together (loadings 0.7 on party, 0.3 and 0.6 on the
#' moderate and conservative positions); the second moves experience and
#' gender (0.4 and 0.8 on some and long experience, -0.2 on the conservative
#' position, 0.6 on a woman candidate).
#'
#' `simconjoint_truth` holds the truth. Its `population` table gives the
#' population value of every quantity the book's simulation chapter
#' estimates: mean tastes, shares of respondents who prefer one level to
#' another, a compensating share, the probability that one candidate beats
#' another, the average marginal effect of long experience, and attribute
#' importance shares. The covariates are discrete (whole years of age from 18
#' to 80, uniform; ideology on seven points; gender with probability one
#' half), so each population value is an exact sum over the 882 covariate
#' cells. Inside a cell, means and shares are exact normal formulas, the
#' contest and the AME are one-dimensional integrals by Gauss-Hermite
#' quadrature with 100 nodes (the AME also enumerates every context the
#' design can produce), and importance shares are two-dimensional integrals
#' by a 256 x 256 Gauss-Hermite rule. The `accuracy` column gives the
#' numerical error: about 1e-12 for the first ten quantities and the change
#' from a 192 x 192 rule (about 1e-6) for importance.
#'
#' The data are built by `data-raw/build_sim.R` in the package sources with a
#' fixed seed.
#'
#' @format `simconjoint`: a data frame with 16,000 rows (800 respondents x 10
#'   tasks x 2 candidates) and 11 columns:
#' \describe{
#'   \item{id, task, profile}{Respondent (1 to 800), task (1 to 10) and
#'     candidate (1 = the first candidate, shown on the left; 2 = the
#'     second).}
#'   \item{party, experience, position, gender}{The candidate's attributes
#'     (factors; the first level is the reference).}
#'   \item{chosen}{1 if this candidate was chosen, 0 if not. The two rows of
#'     a task sum to 1.}
#'   \item{age, ideology, female}{The respondent's characteristics: age in
#'     years (18 to 80), ideology from -3 (very liberal) to 3 (very
#'     conservative), and 1 for women. They repeat on every row of the
#'     respondent.}
#' }
#'
#' `simconjoint_truth`: a list with
#' \describe{
#'   \item{design}{The experiment's design, from [sc_design()].}
#'   \item{parameters}{`kappa`, `omega`, the 6 x 2 `loading` matrix
#'     (`Lambda`) and `q = 2`.}
#'   \item{mean_taste}{The function `mu(age, ideology, female)`, returning
#'     one row of six mean tastes per respondent.}
#'   \item{covariate_law}{The population law of the characteristics.}
#'   \item{respondents}{The 800 respondents' characteristics.}
#'   \item{mu, beta, u, side}{Each respondent's true mean tastes `mu(Z_i)`,
#'     tastes `beta_i` (800 x 6), taste-factor draws `u_i` (800 x 2) and
#'     position effect `a_i`.}
#'   \item{population}{The true population quantities: `name`, `kind`,
#'     `value`, `method` and `accuracy`.}
#'   \item{contrasts}{The coefficient vectors of the contrasts in
#'     `population` (`compensating` is a Republican with no experience
#'     against a Democrat with long experience; `contest` is the coded
#'     difference of the two candidates of the contest).}
#'   \item{seed}{The seed of the simulation.}
#' }
#' @seealso [sc_design()], [sc_code()].
#' @examples
#' data(simconjoint)
#' head(simconjoint)
#' simconjoint_truth$population[, c("name", "value")]
#' des <- simconjoint_truth$design
#' A <- simconjoint[simconjoint$profile == 1, ]
#' B <- simconjoint[simconjoint$profile == 2, ]
#' X <- sc_code(des, A, B)
#' head(X)
"simconjoint"

#' @rdname simconjoint
#' @format NULL
"simconjoint_truth"
