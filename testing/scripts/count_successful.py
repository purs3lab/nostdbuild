import os
import pathlib
import json
import re

def remove_stderr(lines: list[str]):
    res = []
    for line in lines:
        if not re.match(r"\s*\"stderr\"", line):
            res.append(line)
    return res

here = pathlib.Path(__file__).parents[1] / "results"
success = 0
fail = 0
for json_file in os.listdir(str(here)):
    data = None
    print(json_file)
    with open(here / json_file, 'r') as f:
        contents = f.readlines()
        contents = [x.strip("'<>() ").replace('\'', '\"') for x in contents]
        contents = remove_stderr(contents)
        contents = "".join(contents)
        data = json.loads(contents)
    if not data:
        fail += 1
        continue
    for project in data:
        code = int(project["command"]["code"])
        print(f"{project['project'].split("/")[-2]:55}: {code}")
        is_successful = True if code == 0 else False 
        if is_successful:
            success += 1
        else:
            fail += 1
print(f"success: {success}")
print(f"fail: {fail}")
total = success + fail
print(f"percent success: {success / total * 100}")
print(f"percent fail: {fail / total * 100}")