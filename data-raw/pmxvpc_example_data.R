# Generates the small synthetic PK datasets shipped with the package
# (data/pmxvpc_sim_example.rda, data/pmxvpc_obs_example.rda) for use in
# function @examples and the vignette. One-compartment IV bolus model
# with mild between-subject variability on clearance and proportional
# residual error; two dose levels for stratification examples.

set.seed(42)

n_subjects <- 8
n_reps <- 20
times <- c(0, 1, 2, 4, 8, 12, 24)
dose_levels <- c(100, 300)

subjects <- data.frame(
  ID = 1:n_subjects,
  DOSE = rep(dose_levels, each = n_subjects / 2)
)

V <- 20
CL_pop <- 5
subjects$CL <- CL_pop * exp(rnorm(n_subjects, sd = 0.2))

pred_curve <- function(dose, cl, v, t) (dose / v) * exp(-(cl / v) * t)

# Population-only PRED (typical CL, no BSV, no residual error) --
# identical across replicates/subjects at a given DOSE/TIME, matching
# how this package expects PRED to behave (see CLAUDE.md).
pred_grid <- expand.grid(DOSE = dose_levels, TIME = times)
pred_grid$PRED <- pred_curve(pred_grid$DOSE, CL_pop, V, pred_grid$TIME)

# Simulated replicates: each replicate adds its own residual noise on
# top of each subject's individual (BSV-perturbed) curve.
sim <- expand.grid(ID = subjects$ID, REP = 1:n_reps, TIME = times)
sim <- merge(sim, subjects, by = "ID")
sim$IPRED <- pred_curve(sim$DOSE, sim$CL, V, sim$TIME)
sim$DV <- pmax(sim$IPRED * exp(rnorm(nrow(sim), sd = 0.15)), 1e-3)
sim <- merge(sim, pred_grid, by = c("DOSE", "TIME"))
sim <- sim[order(sim$ID, sim$REP, sim$TIME), ]
pmxvpc_sim_example <- sim[, c("ID", "REP", "TIME", "DOSE", "DV", "PRED")]
rownames(pmxvpc_sim_example) <- NULL

# Observed: one realization per subject (no REP column).
obs <- expand.grid(ID = subjects$ID, TIME = times)
obs <- merge(obs, subjects, by = "ID")
obs$IPRED <- pred_curve(obs$DOSE, obs$CL, V, obs$TIME)
obs$DV <- pmax(obs$IPRED * exp(rnorm(nrow(obs), sd = 0.15)), 1e-3)
obs <- merge(obs, pred_grid, by = c("DOSE", "TIME"))
obs <- obs[order(obs$ID, obs$TIME), ]
pmxvpc_obs_example <- obs[, c("ID", "TIME", "DOSE", "DV", "PRED")]
rownames(pmxvpc_obs_example) <- NULL

if (!dir.exists("data")) dir.create("data")
save(pmxvpc_sim_example, file = "data/pmxvpc_sim_example.rda", compress = "bzip2")
save(pmxvpc_obs_example, file = "data/pmxvpc_obs_example.rda", compress = "bzip2")
