# AIRBNB_NEWUSERBOOKINGS
KAGGLE DATASCIENCE COMPETITION  
https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings  

The code is written in R.  
Folder structure:  
> > scripts           - contains feature engineering and modeling scripts  
    xgboost_k3.csv    - submission that scored position 76 on private LB  
    
Important Script files:  
> > featureEngineering_sessions.R ---> generates the session features from session.csv  
    featureEngineering.R          ---> generates the probability for each destination according to the sessions+userprofile features  
    air_definitions.R             ---> Includes all the function definitions that generate the features  
    customfeval.R                  ---> contains the calculation for NCDG metric  
    singleSubmission_hist_Nohist.R ---> Generates the submission file by integrating destination probability data both from users with browsing history/not with history  

I finished 89th on the private LB among the 1463 teams.  

If I had selected the right submisssion ,I would have finished 76 on the private Leaderboard. 

Execution : Place all the data files in the current directory and execute featureEngineering_sessions.R  





    
    
    
  
