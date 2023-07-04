import sys
import os
from pathlib import Path
import subprocess as sp
import time
import datetime as dt

ROOT = os.path.dirname(__file__)

TESTS_DIR = os.path.normpath(os.path.join(ROOT, "tests"))
COMPILER_PATH = os.path.normpath(
    os.path.join(ROOT, "build", "bootstrap", "esl"))

os.chdir(os.path.join(ROOT, "build"))


def load_expectation(path: str):
    # print("loading exp " + path)
    try:
        with open(path, "rb") as f:
            return f.read()
    except Exception as e:
        return ""


failed = 0
passed = 0


def fail(test, cause):
    global failed
    failed += 1
    print(f"\033[41m FAIL \033[0;34m {test}\033[m: {cause}")


def pass_(test, compiler_time):
    global passed
    passed += 1
    print(
        f"\033[42m PASS \033[0;34m {test} \033[30m({compiler_time/1e9:.2f}s)\033[m")


total_start_time = time.perf_counter_ns()

for p in Path(TESTS_DIR).rglob("*.esl"):
    try:
        # print(p)
        expected_out = load_expectation(
            os.path.join(TESTS_DIR, str(p) + ".out"))
        expected_err = load_expectation(
            os.path.join(TESTS_DIR, str(p) + ".err"))

        if len(expected_out) == 0 and len(expected_err) == 0:
            continue

        start_time = time.perf_counter_ns()

        proc = sp.Popen([COMPILER_PATH, str(p)], stdout=sp.PIPE, stderr=sp.PIPE)
        out, err = proc.communicate()
        ret = proc.returncode
        compiler_time = time.perf_counter_ns() - start_time
        # print("out:", out, "err:", err, "ret:", ret)

        if ret != 0:
            if err != expected_err:
                if len(out) > 0:
                    fail(p, "expected success but got compiler error")
                else:
                    fail(p, "got different compile error message than expected")
                continue
        elif len(err) > 0:
            fail(p, "expected compile error but got success")
            continue

        proc = sp.Popen([os.path.join(ROOT, "build", "build", "out")],
                        stdout=sp.PIPE, stderr=sp.PIPE)
        out, err = proc.communicate()
        ret = proc.returncode

        if ret != 0:
            if err != expected_err:
                if len(out) > 0:
                    fail(p, "expected success but got runtime error")
                else:
                    fail(p, "got different runtime error message than expected")
                continue
        elif len(err) > 0:
            fail(p, "expected runtime error but got success")
            continue

        pass_(p, compiler_time)
    except Exception as e:
        fail(p, str(e))

print(
    f"\n\033[32mPassed:\033[m {passed}, \033[31mFailed:\033[m {failed}, Took {(time.perf_counter_ns() - total_start_time)/1e9:.2f}s")

if failed > 0:
    sys.exit(1)
