# pip install keras
# pip install tensorflow
# pip install numpy==1.14 #if needed

# example from Jason Brownlee's tutorial
# https://machinelearningmastery.com/tutorial-first-neural-network-python-keras/

from keras.models import Sequential
from keras.layers import Dense
import numpy

# fix random seed for reproducibility
numpy.random.seed(7)

# load Pima indians data
# codebook: https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.names
df = numpy.loadtxt("pima-indians-diabetes.csv", delimiter=",")

# quick aside about numpy arrays
df.shape # dimensions
df[:2, 1:3] # subset, remember python indexing!
df[1, :] # ":" grabs everything
diabetes_idx = (df[:,8] == 1) # subset based on criteria
diabetes_df = df[diabetes_idx,:] # what did I do?

# subset to train/test sets
train_idx = numpy.random.choice(range(len(df)), size = 500, replace = False)
test_idx = [i for i in range(len(df)) if i not in train_idx]
test_idx.sort()
train_idx.sort()
train_x = df[train_idx,0:8]
train_y = df[train_idx,8]
test_x = df[test_idx,0:8]
test_y = df[test_idx,8]


# 1. Build the model -----------------------------------

# Sequential model...
model = Sequential()

# First layer--INPUT 
#	"input_dim" = 8 because we have 8 covariates
# 	"dense" because we want a fully connected network structure
#	"12" is the number of "neurons" i.e., ouputs of this layer
#	"relu" function within the layers... 
#	Note: default initialization of weights to small number ~ Unif(0, .05) 
model.add(Dense(3, activation = "relu", input_dim = 8))

# Second layer--HIDDEN
model.add(Dense(2, activation='relu'))

# ...

# Third layer--OUTPUT
#	"sigmoid" to ensure output is mapped to between 0 and 1
model.add(Dense(1, activation='sigmoid'))

model.summary()

# 2. Compile the model -------------------------------------

# Define parameters for training
# i.e., finding the best weights to make predictions
#	loss function - used to evaluate a given set of weights
#	optimizer - searches through different weights
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])


# 2. Train the model -------------------------------------

# Define parameters for fitting
#	epoch = iteration over entire training set
#	batch_size = ??
model.fit(train_x, train_y, epochs = 5, batch_size = 10)


# 3. Evaluate on test set -------------------------------------

pred_y = model.predict(test_x)
pred_acc = 0.0
for i, j in zip(pred_y, test_y):
	pred_acc += round(i[0]) == j
pred_acc/len(test_y)

















