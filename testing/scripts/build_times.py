import os
import pathlib
import json
import re
import random
import matplotlib.pyplot as plt
import numpy as np

def get_times() -> tuple[int, int]:
    json_files = os.listdir(str(here))
    build_times = []
    solve_times = []
    for json_file in json_files:
        print(str(json_file))
        data = None
        with open(here / json_file, 'r') as f:
            contents = f.readlines()
            contents = [x.strip("'<>() ").replace('\'', '\"') for x in contents]
            contents = remove_stderr(contents)
            contents = "".join(contents)
            data = json.loads(contents)
        for project in data:
            build_time = project.get("time_to_build", None)
            solve_time = project.get("time_to_solve", None)
            if not (build_time and solve_time):
                continue

            build_times.append(build_time)
            solve_times.append(solve_time)
    return (build_times, solve_times)

def remove_stderr(lines: list[str]):
    res = []
    for line in lines:
        if not re.match(r"\s*\"stderr\"", line):
            res.append(line)
    return res

here = pathlib.Path(__file__).parents[1] / "results"

build_times_path = pathlib.Path(__file__).parents[1] / "metrics"

success = 0
fail = 0
json_files = os.listdir(str(here))
json_files = random.choices(json_files, k=20)

build_times = []
solve_times = []
population = []

for json_file in json_files:
    data = None
    with open(here / json_file, 'r') as f:
        contents = f.readlines()
        contents = [x.strip("'<>() ").replace('\'', '\"') for x in contents]
        contents = remove_stderr(contents)
        contents = "".join(contents)
        data = json.loads(contents)
    for project in data:
        build_time = project.get("time_to_build", None)
        solve_time = project.get("time_to_solve", None)
        if not (build_time and solve_time):
            continue

        print(f"{project['project'].split("/")[-2]:55}: {build_time} {solve_time}")
        build_times.append(build_time)
        solve_times.append(solve_time)

        p = json_file.removesuffix(".json")
        population.append(p.rsplit("-", maxsplit=1)[0])

build_longer = 0
solve_longer = 0
for b, s in zip(*get_times()):
    if b > s:
        build_longer += 1
    else:
        solve_longer += 1
print(f"The build time was longer for {build_longer} crates, but the solve time was longer for {solve_longer} crates")

data = {
    "Solve Times": solve_times,
    "Build Times": build_times,
}

x = np.arange(len(population))  # the label locations
width = 0.3 # the width of the bars
multiplier = 0

fig, ax = plt.subplots(layout='constrained')

for attribute, measurement in data.items():
    offset = width * multiplier
    rects = ax.barh(x + offset, measurement, width, label=attribute)
    multiplier += 1

ax.set_xlabel('Time (ms)')
ax.set_ylabel('Crate')
ax.set_title('Comparing Build Times to Configuration Solve Times')
ax.set_yticks(x + width, population)
# plt.yticks(rotation=45, ha="right")
ax.legend()
plt.savefig(build_times_path / "build_times.png", bbox_inches="tight")
