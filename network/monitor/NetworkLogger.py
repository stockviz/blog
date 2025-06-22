import subprocess, shlex, os
import pickle
from datetime import datetime, timedelta
from time import sleep

networkLogFileName = "C:\\Users\\shyam\\pyscripts\\fubar_fawlty.pickle"

def monitor():
    failPing = []
    try:
        with open(networkLogFileName, 'rb') as f:
            failPing = pickle.load(f)
            print(f"loaded ping data: [{failPing[0]} ~ {failPing[-1]}]")
    except:
        pass

    while True:
        print(".", end="", flush=True)
        resp = subprocess.run(shlex.split("ping -n 1 google.com"), capture_output=True, text=True)

        if resp.returncode != 0:
            now = datetime.now()
            failPing.append(now)
            cutoff = now - timedelta(minutes=5)
            numberOfFails = len([x for x in failPing if x > cutoff])

            print(f"failed: {now}/{numberOfFails}")

            with open(networkLogFileName, 'wb') as f:
                pickle.dump(failPing, f)

        sleep(15)

monitor()

