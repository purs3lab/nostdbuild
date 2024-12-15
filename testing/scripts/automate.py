import sys
import pathlib
import os
import subprocess

if len(sys.argv) != 2:
    print("USAGE: python3 automate.py {directory-with-crates}")
    sys.exit(1)

already = []
with open(pathlib.Path(__file__).parent / "out.txt", "r") as f:
    already = f.readlines()

dir = sys.argv[1]
dirs = [pathlib.Path(dir) / pathlib.Path(entry) for entry in os.listdir(dir) if os.path.isdir(os.path.join(dir, entry))]

for d in dirs:
    if any([x for x in already if str(d) in x]):
        continue
    res = subprocess.run(f"cargo run {d}", shell=True, text=True, capture_output=True, cwd=pathlib.Path(__file__).parent.parent)
    print(f"ran on directory {d}: {res.returncode}")
    if res.returncode != 0:
        print("exiting")
        print(f"stdout: {res.stdout}")
        print(f"stderr: {res.stderr}")
        sys.exit(1)
    # print(f"stdout: {res.stdout}")
    # print(f"stderr: {res.stderr}")