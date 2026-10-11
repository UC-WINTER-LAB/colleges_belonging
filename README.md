Originally analysis produced the latent variables, then conducted path analysis.
This is done in scripts with prefix 00 to 02.

After revise and resubmit the new analysis moves variables back to wide format.
It then conducts latent variable estimation, and path analysis in one model.
It also uses FIML to impute missing values.
This is done in script 03_simic_latent_sem.R

I split 03_simic_latent_sem into a and b:
a - the total effect without mediators
b - the full model with mediators