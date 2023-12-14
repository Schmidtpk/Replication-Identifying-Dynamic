######## Main files

- the dynamic attention model can be found in "./analysis/jags_dyn.R"
- it is used in "./analysis/full_dyn.R" on the experimental data


######## Empirical example (in folder "analysis")

### data

- curated in R-package DynExpData (https://github.com/Schmidtpk/DynExpData)

### full_dyn.R

- applies dynamic model to experimental data
- generated 06_07_22dyn4.RDS

### full_stat.R

- applies static model to experimental data
- generated 13_07_22static4beta.RDS

### full_analysis.R:

- loads estimation results and generates plots for paper
- uses 13_07_22static4beta.RDS,06_07_22dyn4.RDS,CFA_full_loglik.RDS
- generates plots states_example2,statesC,ggrocA5

### Experiment_CFA_CutoffModels.R

- uses standard cutoff methods on experimental data as comparison
- generated CFA_full_loglik.RDS


### post_proc_cutoff_cfa.R

- uses estimation results and generates plots for paper
- generates load_plot,load_SD_plot,cross_tabs,desc_tab


######## Simulation (in folder "simulations")

### gendata_sim2.R

- generates data sets for simulation


### runall_sim.R

- runs estimation in simulations and saves single simulation runs


### getsim100.R

- loops over single runs and saves in data.frames
- generates files loads.rds, persons.rds, states.rds
- generates plots mixing_ran1,mixing_ran2,sim_att_ran2,loads_ran2_overp, loads_ran2





