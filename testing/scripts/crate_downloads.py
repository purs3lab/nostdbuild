import os
import pathlib
import json
import re
import matplotlib.pyplot as plt
import numpy as np

def remove_stderr(lines: list[str]):
    res = []
    for line in lines:
        if not re.match(r"\s*\"stderr\"", line):
            res.append(line)
    return res

results = pathlib.Path(__file__).parents[1] / "results"

def build_result(package: str) -> bool:
    files = os.listdir(results)
    file = [x for x in files if x.startswith(package)]
    file = file[0]
    data = None
    with open(results / file, "r") as f:
        contents = f.readlines()
        contents = [x.strip("'<>() ").replace('\'', '\"') for x in contents]
        contents = remove_stderr(contents)
        contents = "".join(contents)
        data = json.loads(contents)
    result = False
    print(file)
    for project in data:
        code = int(project["command"]["code"])
        result = result or code == 0

    return result

metrics = pathlib.Path(__file__).parents[1] / "metrics"
packages = []
with open(metrics / "downloads.txt", "r") as f:
    for line in f.readlines():
        package, downloads = line.strip("\n").split(" ")
        packages.append((package, int(downloads)))
print(packages)
packages = sorted(packages, key=lambda x: x[1], reverse=True)

for i in range(len(packages)):
    package, downloads = packages[i]
    res = build_result(package)
    packages[i] = (package, downloads, res)
print(packages)

i = 0
j = 0
print()
print("top 10 packages for which we found a build command")
top_10 = []
for package, downloads, found_build in packages:
    if j == 20:
        break
    if found_build:
        print(f"{package:20}: {downloads}")
        top_10.append((package, downloads))
        j += 1
    i += 1
fig, ax = plt.subplots()
y_pos = np.arange(len(top_10))
ax.barh(y_pos, [d[1] for d in top_10], align='center')
ax.set_yticks(y_pos, labels=[d[0] for d in top_10])
# plt.yticks(rotation=45, ha='right')
ax.invert_yaxis()  # labels read top-to-bottom
ax.set_xlabel('Downloads')
ax.set_ylabel('Crate')
ax.set_title('Top Crates by Downloads with Successful Builds')
plt.savefig(metrics / "top-found-config.png", bbox_inches="tight")

print()
good = 0
bad = 0
top_20 = []
for package, downloads, found_build in packages[:20]:
    print(f"{package:20} {downloads:10} {found_build}")
    top_20.append((package, downloads, found_build))
    if found_build:
        good += 1
    else:
        bad += 1

print(f"of the top 20 most downloaded, we found {good} build configs successfully")
print()
fig, ax = plt.subplots()
bar_labels = []
bar_colors = []
successful = "Successful"
failure = "Failure"
for _, _, good in top_20:
    if good:
        bar_labels.append(successful)
        successful = "_Successful"
        bar_colors.append('tab:blue')
    else:
        bar_labels.append(failure)
        failure = "_Failure"
        bar_colors.append('tab:red')
ax.barh([x[0] for x in top_20], [x[1] for x in top_20], label=bar_labels, color=bar_colors)
plt.xticks(rotation=45, ha='right')
ax.invert_yaxis()  # labels read top-to-bottom
ax.set_xlabel('Downloads')
ax.set_ylabel('Crate')
ax.set_title('Top Crates by Downloads')
ax.legend(title='Build Result')
plt.savefig(metrics / "top-downloads.png", bbox_inches = "tight")


good = 0
bad = 0
top_20 = []
for package, downloads, found_build in packages[-20:]:
    print(f"{package:20} {downloads:10} {found_build}")
    top_20.append((package, downloads, found_build))
    if found_build:
        good += 1
    else:
        bad += 1
print(f"of the bottom 20 least downloaded, we found {good} build configs successfully")
fig, ax = plt.subplots()
bar_labels = []
bar_colors = []
successful = "Successful"
failure = "Failure"
for _, _, good in top_20:
    if good:
        bar_labels.append(successful)
        successful = "_Successful"
        bar_colors.append('tab:blue')
    else:
        bar_labels.append(failure)
        failure = "_Failure"
        bar_colors.append('tab:red')
ax.barh([x[0] for x in top_20], [x[1] for x in top_20], label=bar_labels, color=bar_colors)
plt.xticks(rotation=45, ha='right')
ax.invert_yaxis()  # labels read top-to-bottom
ax.set_xlabel('Downloads')
ax.set_ylabel('Crate')
ax.set_title('Bottom Crates by Downloads')
ax.legend(title='Build Result')
plt.savefig(metrics / "bottom-downloads.png", bbox_inches = "tight")
