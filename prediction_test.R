#1. import data.Rating by file "OpenFile.R"
#2. decompose data.Rating into TrainingU and TestU


stat_U = stat_Users(TrainingU1);
stat_M = stat_Movies(TrainingU1);
ResultTest = prediction_naive(stat_U,stat_M, TestU1)
