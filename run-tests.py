import sys
import os
from pathlib import Path
import subprocess as sp
import time
import datetime as dt
import argparse as ap
from concurrent.futures import ThreadPoolExecutor

parser = ap.ArgumentParser("run-tests")
parser.add_argument(
    "--update", help="update expectations with actual outputs", action="store_true")
args = parser.parse_args()

ROOT = os.path.dirname(__file__)

TESTS_DIR = os.path.normpath(os.path.join(ROOT, "tests"))
COMPILER_PATH = os.path.normpath(
    os.path.join(ROOT, "build", "bootstrap", "esl"))

BINARY_DIR = os.path.join(ROOT, "build")
os.chdir(BINARY_DIR)


def load_expectation(path: str):
    # print("loading exp " + path)
    try:
        with open(path, "rb") as f:
            return f.read()
    except Exception as e:
        return ""


failed = 0
passed = 0


def fail(test_path, cause):
    test_name = test_path.relative_to(TESTS_DIR)
    global failed
    failed += 1
    print(f"\033[41m FAIL \033[0;34m {test_name}\033[m: {cause}")


def pass_(test_path, compiler_time):
    test_name = test_path.relative_to(TESTS_DIR)
    global passed
    passed += 1
    print(
        f"\033[42m PASS \033[0;34m {test_name} \033[30m({compiler_time/1e9:.2f}s)\033[m")


total_start_time = time.perf_counter_ns()


def run_test(test_path, env_dir_id):
    try:
        env_dir = os.path.join(BINARY_DIR, str(env_dir_id))
        os.makedirs(env_dir, exist_ok=True)

        # print(p)
        expected_out_path = os.path.join(TESTS_DIR, str(test_path) + ".out")
        expected_out = load_expectation(expected_out_path)

        expected_err_path = os.path.join(TESTS_DIR, str(test_path) + ".err")
        expected_err = load_expectation(expected_err_path)

        if len(expected_out) == 0 and len(expected_err) == 0:
            return

        start_time = time.perf_counter_ns()

        proc = sp.Popen([COMPILER_PATH, str(test_path)],
                        stdout=sp.PIPE, stderr=sp.PIPE,
                        cwd=env_dir)
        out, err = proc.communicate()
        ret = proc.returncode
        compiler_time = time.perf_counter_ns() - start_time
        # print("out:", out, "err:", err, "ret:", ret)

        if args.update:
            if len(err) > 0:
                with open(expected_err_path, "wb") as f:
                    f.write(err)
            if ret != 0:
                print(f"compiler error: {test_path}")
                return
        else:
            if ret != 0:
                if err != expected_err:
                    sys.stderr.buffer.write(err)
                    sys.stderr.buffer.flush()
                    if len(out) > 0:
                        fail(test_path, "expected success but got compiler error")
                    else:
                        fail(
                            test_path, "got different compile error message than expected")
                pass_(test_path, compiler_time)
                return
            elif len(err) > 0:
                fail(test_path, "expected compile error but got success")
                return

        proc = sp.Popen([os.path.join(env_dir, "build", "out")],
                        stdout=sp.PIPE, stderr=sp.PIPE, cwd=env_dir)
        out, err = proc.communicate()
        ret = proc.returncode

        if args.update:
            if len(err) > 0:
                with open(expected_err_path, "wb") as f:
                    f.write(err)
            if len(out) > 0:
                with open(expected_out_path, "wb") as f:
                    f.write(out)
        else:
            if ret != 0:
                if err != expected_err:
                    sys.stderr.buffer.write(err)
                    sys.stderr.buffer.flush()
                    if len(out) > 0:
                        fail(test_path, "expected success but got runtime error")
                    else:
                        fail(
                            test_path, "got different runtime error message than expected")
                    return
            elif len(out) > 0:
                if out != expected_out:
                    fail(test_path, "got different output than expected")
                    return
            elif len(err) > 0:
                fail(test_path, "expected runtime error but got success")
                return

        pass_(test_path, compiler_time)
    except Exception as e:
        fail(test_path, str(e))


with ThreadPoolExecutor() as pool:
    for idx, test_path in enumerate(Path(TESTS_DIR).rglob("*.esl")):
        pool.submit(run_test, test_path, idx)

print(
    f"\n\033[32mPassed:\033[m {passed}, \033[31mFailed:\033[m {failed}, Took {(time.perf_counter_ns() - total_start_time)/1e9:.2f}s")

if failed > 0:
    sys.exit(1)
