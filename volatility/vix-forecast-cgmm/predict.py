import pyodbc
import numpy as np
import pandas as pd
import csv

from sklearn.mixture import GaussianMixture
from sklearn.preprocessing import StandardScaler

from cgmm import GMMConditioner, ConditionalGMMRegressor

from Config import Config

with pyodbc.connect(Config['NORWAY']) as norway:
    with norway.cursor() as cursor:
        sqlResult = cursor.execute("select px_close, time_stamp from vix_history order by time_stamp").fetchall()

df = pd.DataFrame.from_records(sqlResult, columns=['CLOSE', 'DATE'])

##Compute x=log(VIX) and it’s daily change
log_vix = np.log(df["CLOSE"].astype(float).to_numpy())
x = log_vix[:-1]
dx = log_vix[1:] - log_vix[:-1]

pairs = np.column_stack((x, dx))

##Monte Carlo path simulation
def simulate_paths_delta(cond, x0, horizon=60, n_paths=100, seed=123):
    rng = np.random.default_rng(seed)
    xs = np.empty((n_paths, horizon + 1), dtype=float)

    for i in range(n_paths):
        xt = float(x0)
        xs[i, 0] = xt
        for t in range(horizon):
            gmmd = cond.condition([xt])  # posterior over Δ (1D GMM)
            gmmd.random_state = int(rng.integers(0, 2**32 - 1))
            y, _ = gmmd.sample(1)
            xt = xt + float(y[0, 0])     # x_{t+1} = x_t + Δ
            xs[i, t + 1] = xt
    return xs


##fit the model and predict. expanding window.
look_back = 500
K = 3
H, N = 20, 100

print(f"training over {len(pairs)} days")

predictions = []
for i in range(look_back, len(pairs) - 1): #look_back + 20
    if i % 100 == 0:
        print(i, end="/", flush=True)

    gmm = GaussianMixture(n_components=K, covariance_type="full", random_state=0).fit(pairs[(i-look_back):i])
    cond = GMMConditioner(gmm, cond_idx=[0], reg_covar=1e-9).precompute()

    # Run scenarios starting from last observed x

    xs_paths = simulate_paths_delta(cond, x0=pairs[i + 1, 0], horizon=H, n_paths=N)
    vix_paths = np.exp(xs_paths)  # back to VIX level
    q = np.quantile(vix_paths, [0.1, 0.5, 0.9], axis=0)
    median_pred = q[1] #an array of predictions
    as_of = df.iloc[i+1, 1]
    predictions.append((as_of, median_pred))


with open("cgmm-vix-prediction.csv", 'w') as file:
    for pred in predictions:
        predStr = ','.join([str(x.astype(float)) for x in pred[1]])
        file.write(pred[0].isoformat() + ',' + predStr + '\n')