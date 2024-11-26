# max_coverage_relocation with glpk returns correct names

    Code
      mc_relocate_glpk
    Output
      
      ----------------------------------------- 
      Model Fit: maxcovr relocation model 
      ----------------------------------------- 
      model_used:        max_coverage_relocation 
      existing_facility: york_selected 
      proposed_facility: york_unselected 
      user:              york_crime 
      distance_cutoff:   100 
      cost_install:      500 
      cost_removal:     100 
      cost_total:        1000 
      solver:            glpk 
      -----------------------------------------

# max_coverage_relocation with lpSolve returns correct names

    Code
      mc_relocate_lpsolve
    Output
      
      ----------------------------------------- 
      Model Fit: maxcovr relocation model 
      ----------------------------------------- 
      model_used:        max_coverage_relocation 
      existing_facility: york_selected 
      proposed_facility: york_unselected 
      user:              york_crime 
      distance_cutoff:   100 
      cost_install:      500 
      cost_removal:     100 
      cost_total:        1000 
      solver:            lpSolve 
      -----------------------------------------

