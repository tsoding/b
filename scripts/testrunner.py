from pathlib import Path
import subprocess
import difflib

p = Path(".") / "tests"

failed = []

subprocess.run(["make", "test"], capture_output=True)

print("== RESULTS ==")
for f in (p / "expected").glob('*'):
    with open(f"./tests/expected/{f.name}", 'r') as fp:
        try:
            proc = subprocess.run([f"./tests/build/{f.name}"], capture_output=True)
            stdout = proc.stdout.decode().strip()
        except subprocess.CalledProcessError as e:
            stdout = str(e)

        expected = fp.read().strip()
        if (expected == stdout):
            print(".", end="")
        else:
            print("x", end="")
            failed.append({
                "stdout": stdout,
                "expected": expected,
                "test": f.name
            })

print("")

for fail in failed:
    print(f"=== {fail['test']} Failed ===")
    print("".join(difflib.ndiff(fail['stdout'], fail['expected'])))
else:
    print("OK")
