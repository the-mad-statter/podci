# Q1	Lift heavy books?
# Q2	Pour a half gallon of milk?
# Q3	Open a jar that has been opened before?
# Q4	Use a fork and spoon?
# Q5	Comb his/her hair?
# Q6	Button buttons?
# Q8	Write with a pencil?
# Q32	Turn door knobs?
podci_parent_upper_extremity <- function(
  podci_parent_items,
  score = c('mean', 'standard', 'normative'),
  reference_mean = 0, # peds = 91.9138; ado = 98.73970
  reference_sd = 1    # peds = 11.4968; ado = 5.03207
) {
  score <- match.arg(score)

  podci_parent_items %>%
    select(all_of(paste0('Q', c(1:6, 8, 32)))) %>%
    rowwise() %>%
    mutate(
      across(everything(), ~ if_else(. == 5, NA_real_, .)),
      n_obs = sum(!is.na(c_across())),
      mean = if_else(n_obs >= 4, mean(c_across(-n_obs), na.rm = TRUE), NA_real_),
      standard = ((4 - mean) / 3) * 100,
      normative = 10 * ((standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}

# Q7	Put on his/her coat?
# Q21	Climb one flight of stairs?
# Q24	Walk one block?
# Q25	Get on and off a bus?
# Q28	Stand while washing his/her hands and face at a sink?
# Q29	Sit in a regular chair without holding on?
# Q30	Get on and off a toilet or chair?
# Q31	Get in and out of bed?
# Q33	Bend over from a standing position and pick up something off the floor?
# Q34	How often does your child need help from another person for sitting and standing?
# Q35	How often does your child use assistive devices (such as braces, crutches, or wheelchair) for sitting and standing?
podci_parent_transfer <- function(
  podci_parent_items,
  score = c('mean', 'standard', 'normative'),
  reference_mean = 0, # ped = 98.3346; ado = 99
  reference_sd = 1    # ped = 5.69935; ado = 5
) {
  score <- match.arg(score)

  podci_parent_items %>%
    select(all_of(paste0('Q', c(7, 21, 24, 25, 28:31, 33:35)))) %>%
    rowwise() %>%
    mutate(
      across(everything(), ~ if_else(. == 5, NA_real_, .)),
      n_obs = sum(!is.na(c_across())),
      across(c(Q34, Q35), ~ ((. - 1) * 3 / 4) + 1),
      mean = if_else(n_obs >= 5, mean(c_across(-n_obs), na.rm = TRUE), NA_real_),
      standard = ((4 - mean) / 3) * 100,
      normative = 10 * ((standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}

# Q18	Run short distances?
# Q19	Bicycle or tricycle?
# Q20	Climb three flights of stairs?
# Q22	Walk more than a mile?
# Q23	Walk three blocks?
# Q26	How often does your child need help from another person for walking and climbing?
# Q27	How often does your child use assistive devices (such as braces,  crutches,  or wheelchair) for walking and climbing?
# Q36	Can your child participate in recreational outdoor activities with other children the same age?
# 	Note: conditional on Q42 Too young?
# 	Note: conditional on Q43 Activity not in season?
# Q44	Can your child participate in pickup games or sports with other children the same age?
# 	Note: conditional on Q50 Too young?
# 	Note: conditional on Q51 Activity not in season?
# Q52	Can your child participate in competitive level sports with other children the same age?
# 	Note: conditional on Q58 Too young?
# 	Note: conditional on Q59 Activity not in season?
# Q60	How often in the last week did your child get together and do things with friends?
# 	Note: conditional on Q65 Friends not around?
# Q66	How often in the last week did your child participate in gym/recess?
# 	Note: conditional on Q72 School not in session?
# 	Note: conditional on Q73 Does not attend school?
podci_parent_sports <- function(
  podci_parent_items,
  score = c('mean', 'standard', 'normative'),
  reference_mean = 0, # ped = 92.44210; ado = 94
  reference_sd = 1    # ped = 10.21742; ado = 11
) {
  score <- match.arg(score)
  scale_items <- c(18:20, 22, 23, 26, 27, 36, 44, 52, 60, 66)
  scale_dependencies <- c(42, 43, 50, 51, 58, 59, 65, 72, 73)

  `%==%` <- function(lhs, rhs) {
    if(is.na(lhs))
      return(FALSE)
    else
      return(lhs == rhs)
  }

  podci_parent_items %>%
    select(all_of(paste0('Q', c(scale_items, scale_dependencies)))) %>%
    rowwise() %>%
    mutate(
      across(all_of(paste0('Q', scale_items)), ~ if_else(. == 5, NA_real_, .)),
      n_obs = sum(!is.na(c_across(all_of(paste0('Q', scale_items))))),
      across(c(Q26, Q27), ~ ((. - 1) * 3 / 4) + 1),
      Q36 = if_else(Q36 == 4 & (Q42 %==% 1 | Q43 %==% 1), NA_real_, Q36),
      Q44 = if_else(Q44 == 4 & (Q50 %==% 1 | Q51 %==% 1), NA_real_, Q44),
      Q52 = if_else(Q52 == 4 & (Q58 %==% 1 | Q59 %==% 1), NA_real_, Q52),
      Q60 = if_else(Q60 == 3 &  Q65 %==% 1, NA_real_, ((Q60 - 1) * 3 / 2) + 1),
      Q66 = if_else((Q66 == 4) | (Q66 == 3 & (Q72 %==% 1 | Q73 %==% 1)), NA_real_, ((Q66 - 1) * 3 / 2) + 1),
      mean = if_else(n_obs >= 6, mean(c_across(all_of(paste0('Q', scale_items))), na.rm = TRUE), NA_real_),
      standard = ((4 - mean) / 3) * 100,
      normative = 10 * ((standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}

# Q17	Did pain or discomfort interfere with your child’s activities?
# Q75	How much pain has your child had during the last week?
# Q76	During the last week,  how much did pain interfere with your child’s normal activities (including at home,  outside of the home,  and at school)?
podci_parent_pain <- function(
  podci_parent_items,
  score = c('mean', 'standard', 'normative'),
  reference_mean = 0, # ped = 92.45370; ado = 88
  reference_sd = 1    # ped = 13.76303; ado = 17
) {
  score <- match.arg(score)

  podci_parent_items %>%
    select(all_of(paste0('Q', c(17, 75, 76)))) %>%
    rowwise() %>%
    mutate(
      n_obs = sum(!is.na(c_across())),
      Q17 = ((4 - Q17) * 4 / 3) + 1,
      Q75 = ((Q75 - 1) * 4 / 5) + 1,
      mean = if_else(n_obs >= 2, mean(c_across(-n_obs), na.rm = TRUE), NA_real_),
      standard = ((4 - (mean - 1)) / 4) * 100,
      normative = 10 * ((standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}

# Q10	How he/she looks?
# Q11	His/her body?
# Q12	What clothes or shoes he/she can wear?
# Q13	His/her ability to do the same things his/her friends do?
# Q14	His/her health in general?
podci_parent_happiness <- function(
  podci_parent_items,
  score = c('mean', 'standard', 'normative'),
  reference_mean = 0, # ped = 89.79410; ado = 82
  reference_sd = 1    # ped = 14.10834; ado = 18
) {
  score <- match.arg(score)

  podci_parent_items %>%
    select(all_of(paste0('Q', 10:14))) %>%
    rowwise() %>%
    mutate(
      across(everything(), ~ if_else(. == 6, NA_real_, .)),                        # peds 6 is too young
      n_obs = sum(!is.na(c_across())),
      mean = if_else(n_obs >= 3, mean(c_across(-n_obs), na.rm = TRUE), NA_real_),
      standard = ((5 - mean) / 4) * 100,
      normative = 10 * ((standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}

podci_parent_global <- function(
  podci_parent_items,
  score = c('standard', 'normative'),
  reference_mean = 0, # ped = 93.8786; ado = 95
  reference_sd = 1    # ped = 7.39539; ado = 7
) {
  score <- match.arg(score)

  podci_parent_items %>%
    transmute(
      upper_extremity_standard = podci_parent_upper_extremity(., 'standard'),
      mobility_standard = podci_parent_transfer(., 'standard'),
      sports_standard = podci_parent_sports(., 'standard'),
      pain_standard = podci_parent_pain(., 'standard')
    ) %>%
    rowwise() %>%
    mutate(
      standard = mean(c_across()),
      normative = (10 * (standard - reference_mean) / reference_sd) + 50
    ) %>%
    pull(score)
}
