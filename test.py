#               run all of the tests:  python test.py 
# record all of the expected outputs:  python test.py record

import subprocess
import sys
import os
from glob import glob
from typing import Callable

def error(path: str, error_message: str) -> None:
    print(f"\n{path} {error_message}")
    sys.exit(1)

def compile_file(path: str) -> None:
    if subprocess.run(["mono", "umi.exe", path]).returncode:
        error(path, "failed to compile")

def expected_path(path: str) -> str:
    return f"{os.path.splitext(path)[0]}.expected"

def test_output(path: str, output: str) -> None:
    with open(expected_path(path), "r") as f:
        expected = f.read()
    if expected != output:
        print(output)
        error(path, "created the above output, which was unexpected")

def record_output(path: str, output: str) -> None:
    with open(expected_path(path), "w") as f:
        f.write(output)

def affect_all(output_func: Callable[[str, str], None]) -> None:
    for directory in ["examples", "tests"]:
        for path in glob(f"{directory}/*.umi"):
            compile_file(path)
            # I'm using a 0 as the input because probably the only 
            # other thing with input I will test is a truth machine
            output = subprocess.check_output(["mono", "output.exe"], input="0", text=True)
            output_func(path, output)

if __name__ == "__main__":
    args = sys.argv[1:]
    subprocess.run(["mcs", "umi.cs"], stdout=subprocess.DEVNULL, check=True)
    if len(args) == 0:
        affect_all(test_output)
        print("Everything passed succeessfully")
    else:
        affect_all(record_output)
        print("Finished recording output successfully")
