import subprocess

urls_tags = []
with open("nostd_failed_build_urls.txt", "r") as f:
    urls_tags = [x.strip("\n").split(" ") for x in f.readlines()]

print(urls_tags)

for url, tag in urls_tags:
    if url == "None": continue
    if tag != "None":
        result = subprocess.run(f"git clone {url} --branch {tag}", shell=True, capture_output=True)
    else:
        result = subprocess.run(f"git clone {url}", shell=True, capture_output=True)
