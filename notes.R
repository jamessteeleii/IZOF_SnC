# Study 1 is similar to study 2 but the latter has more granular scale and also objective performance

# Try to pull relevant priors from study one

# Study 2
# Each sheet a different timepoint - time varies between and within id so treat as just replicates
# Column names are "modality_functionality_intensity/perceivedintensity" e.g., P+I is positive functional intensity
# for clarity "modality" is the domain on the IZOF questionnaire (PBS scale)
# Includes objective performance measures and also self-referenced rating on 11 pt scale

## Questions

# All z-scored

# All include relevant random effects i.e., participant, performance type (strength, power, endurance)
# Report the variances for performance type

# Effect of modality on performances and the relationships between them.
self_ref_performance ~ modality_intensity
objective_performance ~ modality_intensity

(self_ref_performance + objective_performance) ~ modality_intensity

# Relationships between modality intensity and modality perceived impact 
# NOTE these are pre measures, so prior belief in impact of modality intensity

(modality_intensity + modality_perceived_impact) ~ 1 


# Study 3
# Each sheet a different timepoint - time varies between and within id so treat as just replicates
# Column names are "modality_functionality_intensity/perceivedintensity" e.g., P+I is positive functional intensity
# for clarity "modality" is the domain on the IZOF questionnaire (PBS scale)
# Includes only self-referenced rating on 11 pt scale

## Questions

# All z-scored

# All include relevant random effects i.e., participant, performance type (strength, power, endurance)
# Report the variances for performance type

# Effect of modality on performances and the relationships between them.
self_ref_performance ~ modality_intensity

# Relationships between modality intensity and modality perceived impact 
# NOTE these are pre measures, so prior belief in impact of modality intensity

(modality_intensity + modality_perceived_impact) ~ 1 


# Do a internal "meta-analysis" of study 2/3 to explore performance types with more power
# This time include performance types as a fixed effect


# Study 4
# Control vs positive feedback vs technical cooaching vs music

# Standard pre-post control experiment for each interventions effect on modality_intensity and modality_perceived impact
(modality_intensity + modality_perceived_impact) ~ time + time:intervention 

# Effect of modality on performances and the relationships between them.
(self_ref_performance + objective_performance) ~ modality_intensity + intervention_condition + interaction?
  
  





