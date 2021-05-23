#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 27 15:33:42 2021

@author: dayi
"""
import tensorflow as tf
import pandas as pd
import numpy as np
from tensorflow import keras

import os
import tempfile
from datetime import datetime
from packaging import version


import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.metrics import accuracy_score
import sklearn
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

"read data"

df = pd.read_csv('/Users/dayi/資料/原始資料集/pd_speech_features_754by756class2.csv')
df=df.rename(columns={df.iloc[:,(df.shape[1]-1)].name : "class"})

df["class"]=df["class"].factorize()[0]


"preprocessing data"
train_df, test_df = train_test_split(df, test_size=0.2)
train_df, val_df = train_test_split(train_df, test_size=0.2)


train_labels = np.array(train_df.pop('class'))
test_labels = np.array(test_df.pop('class'))
val_labels = np.array(val_df.pop('class'))

train_features = np.array(train_df)
test_features = np.array(test_df)
val_features = np.array(val_df)

dim=train_features.shape[1]

scaler = StandardScaler()
train_features = scaler.fit_transform(train_features)

val_features = scaler.transform(val_features)
test_features = scaler.transform(test_features)

train_features = np.clip(train_features, -5, 5)
val_features = np.clip(val_features, -5, 5)
test_features = np.clip(test_features, -5, 5)


"""
LSTM 需要將資料轉成3 dim (batch_size, timesteps, input_dim)
"""
train_features=train_features.reshape(-1,1,dim)
val_features = val_features.reshape(-1,1,dim)
test_features = test_features.reshape(-1,1,dim)


#train_labels = train_labels.reshape(-1,1,len(df["class"].unique()))
#test_labels = test_labels.reshape(-1,1,len(df["class"].unique()))
#val_labels = val_labels.reshape(-1,1,len(df["class"].unique()))


"build model"

model = keras.Sequential()
model.add(keras.layers.LSTM(100, input_shape=(1, dim), return_sequences=True))
model.add(keras.layers.LSTM(56, return_sequences=True))
model.add(keras.layers.Dense(units=len(df["class"].unique())))
model.compile(loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True), optimizer="adam", metrics= ['accuracy'])


model_checkpoint_callback = tf.keras.callbacks.ModelCheckpoint(
    filepath='/Users/dayi/PycharmProjects/LSTM_model_'+format(datetime.now().strftime("%Y.%m.%d.%H.%M.%S")),
    save_weights_only=True,
    monitor='val_accuracy',
    mode='max',
    save_best_only=True)

logdir = '/Users/dayi/PycharmProjects/LSTM_model_'+format(datetime.now().strftime("%Y.%m.%d.%H.%M.%S"))

tensorboard_callback = tf.keras.callbacks.TensorBoard(log_dir=logdir)

history = model.fit(train_features,train_labels,batch_size=8,epochs=50, validation_data=(val_features,val_labels),callbacks=[tensorboard_callback,model_checkpoint_callback])

model.load_weights(logdir)

predict=model.predict_classes(test_features)
acc=accuracy_score(test_labels,predict)

print("test_predict:",predict.flatten())
print("test_true   :",test_labels)
print("accuracy:",acc)
