

A short description of relevant files
1. gen_data folder: folder full of generated data 
2. utils.py: common functions and function classes across several experiments 
3. utils.R: common function across several experiments 
4. python_impute_helpers.py: python script to use python imputation methods in R 

Experiments

Case Study 1 
- experiment_pipeline.py

Case Study 2 
1. Stability of Conditional Independence Test Statistics in a Mixed Graph Setting
    a. permutation score: exp2_permutation.R
    b. z-score: z_score_stability.R
2. Stability of CITS Under Different Imputation Schema 
    a. permutation score: exp2_permutation.R 
    b. zscore: z_score_stability.R
    c. graph generation: exp2_permutation.R, cits_stability_pcalgo.py
3. Causally Informed MICE Z-Score Stability 
    a. caseStudy2_exp3.R 
4. Z-Score Stability Under MNAR Missingness 
    a. case_study2_exp4.R
    b. caseStudy2_exp4_graph_gen.ipynb

Case Study 3 
1. Prediction Method Testing for Causally Informed MICE
    a. caseStudy3_exp1.R 
2. Causally Informed MICE in a Larger Graph Setting 
    a. caseStudy3_exp2.py 