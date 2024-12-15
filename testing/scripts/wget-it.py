import subprocess
import pathlib

here = pathlib.Path(__file__).parent
files = [
    here / "nostd_failed_build_urls.txt",
    here / "nostd_failed_build_urls_2.txt",
]

for f in files:
    with open(str(f), "r") as fp:
        urls = [x.strip("\n") for x in fp.readlines()]
        for url in urls:
            subprocess.run(f"wget {url}", text=True, shell=True, cwd=str(here / "crates"))
            subprocess.run("tar -xf download", text=True, shell=True, cwd=str(here / "crates"))
            subprocess.run("rm download", text=True, shell=True, cwd=str(here / "crates"))