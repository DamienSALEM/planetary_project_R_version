import random
from flask import Flask
import pandas as pd
import numpy as np

app=Flask("Space_Probe")

@app.get("/")
def home():
    return {"message":"Hello World"}

@app.get("/lastscan")
def last_scan():
    data=pd.read_csv("api/flaskapp/data/hwc.csv")
    x=random.randrange(0,data.index[-1])
    res={}
    res["planete_number"]=x
    for col in data.columns:
        if type(data[col][x])==np.float64:
            res[col]=float(data[col][x])
        elif type(data[col][x])==np.int64:
            res[col]=int(data[col][x])
        else:
            res[col]=data[col][x]
    return res
    

if __name__ == "__main__":
    app.run(port=5000)

