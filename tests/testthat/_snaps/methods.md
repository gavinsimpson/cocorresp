# print and summary methods dispatch for every supported class

    Code
      print(summary(model_fixture("predictive")))
    Output
      
      Predictive Co-Correspondence Analysis
      
      Call: predcoca.simpls(y = y, x = x, R0 = weights, n.axes = n.axes,
      nam.dat = nam.dat)
      
      Percentage Variance Explained:
      
      Y-block: variance explained in d$y (response) 
                   Comp 1  Comp 2  Comp 3
      Individual:   5.579   3.620   4.071
      Cumulative:   5.579   9.198  13.269
      
      X-block: variance explained in d$x (predictor) 
                   Comp 1  Comp 2  Comp 3
      Individual:  38.37   26.30   13.08 
      Cumulative:  38.37   64.67   77.74 

