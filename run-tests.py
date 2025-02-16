import sys
import os
from pathlib import Path
import subprocess as sp
import time
import datetime as dt
import argparse as ap
from concurrent.futures import ThreadPoolExecutor

parser = ap.ArgumentParser("run-tests")
parser.add_argument("--update", help="update expectations for matching tests with actual outputs")
args = parser.parse_args()

ROOT = os.path.dirname(__file__)

TESTS_DIR = os.path.normpath(os.path.join(ROOT, "tests"))
COMPILER_PATH = os.path.normpath(
    os.path.join(ROOT, "rust/target/debug/elc"))

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

def get_expected_paths(test_path):
    # Compile error (stdout)
    expected_compile_out_path = os.path.join(TESTS_DIR, str(test_path) + ".cerr")
    # Runtime stdout
    expected_out_path = os.path.join(TESTS_DIR, str(test_path) + ".out")
    # Runtime stderr (e.g panics)
    expected_err_path = os.path.join(TESTS_DIR, str(test_path) + ".err")
    return expected_compile_out_path, expected_out_path, expected_err_path


def run_compiler(test_path, env_dir):
    proc = sp.Popen([COMPILER_PATH, str(test_path)],
                    stdout=sp.PIPE, stderr=sp.PIPE,
                    cwd=env_dir)
    compile_out, compile_err = proc.communicate()
    ret = proc.returncode

    return compile_out, compile_err, ret


def run_runtime(test_path, env_dir):
    proc = sp.Popen([os.path.join(env_dir, "build", "out")],
                    stdout=sp.PIPE, stderr=sp.PIPE, cwd=env_dir)
    out, err = proc.communicate()
    ret = proc.returncode

    return out, err, ret


def run_test(test_path, env_dir_id):
    try:
        env_dir = os.path.join(BINARY_DIR, str(env_dir_id))
        os.makedirs(env_dir, exist_ok=True)

        expected_compile_out_path, expected_out_path, expected_err_path = get_expected_paths(test_path)
        expected_compile_out = load_expectation(expected_compile_out_path)
        expected_out = load_expectation(expected_out_path)
        expected_err = load_expectation(expected_err_path)

        if len(expected_compile_out) == 0 and len(expected_out) == 0 and len(expected_err) == 0:
            return

        start_time = time.perf_counter_ns()

        # Compiler
        compile_out, compile_err, ret = run_compiler(test_path, env_dir)
        compiler_time = time.perf_counter_ns() - start_time
        # print("out:", out, "err:", err, "ret:", ret)

        if ret != 0:
            if len(expected_compile_out) > 0:
                if compile_out != expected_compile_out:
                    # print(f"expected: {expected_compile_out}")
                    # print(f"got: {compile_out}")
                    fail(test_path, "got different compiler error than expected")
                    return
                else:
                    pass_(test_path, compiler_time)
                    return
            else:
                fail(test_path, "expected success but got compiler error")
                return
        elif len(expected_compile_out) > 0:
            fail(test_path, "expected compiler error but got success")
            return

        # Runtime
        out, err, ret = run_runtime(test_path, env_dir)

        if ret != 0:
            if len(expected_err) > 0:
                if err != expected_err:
                    # print(f"expected: {expected_err}")
                    # print(f"got: {err}")
                    fail(test_path, "got different runtime error message than expected")
                    return
                else:
                    pass_(test_path, compiler_time)
                    return
            else:
                fail(test_path, "expected success but got runtime error")
                return
        elif len(expected_err) > 0:
            fail(test_path, "expected runtime error but got success")
            return

        pass_(test_path, compiler_time)
    except Exception as e:
        fail(test_path, str(e))


def update_test(test_path, env_dir_id):
    cout_path, out_path, err_path = get_expected_paths(test_path)

    env_dir = os.path.join(BINARY_DIR, str(env_dir_id))
    os.makedirs(env_dir, exist_ok=True)

    # compiler
    compile_out, compile_err, ret = run_compiler(test_path, env_dir)
    if ret != 0:
        with open(cout_path, "wb") as f:
            f.write(compile_out)
        return

    # runtime
    out, err, ret = run_runtime(test_path, env_dir)
    if len(out) > 0:
        with open(out_path, "wb") as f:
            f.write(out)
    if len(err) > 0:
        with open(err_path, "wb") as f:
            f.write(err)


if args.update:
    update_test(TESTS_DIR / Path(args.update), 0)
else:
    MAX_THREADS = 1
    with ThreadPoolExecutor(max_workers=MAX_THREADS) as pool:
        for idx, test_path in enumerate(Path(TESTS_DIR).rglob("*.esl")):
            pool.submit(run_test, test_path, idx)

    print(
        f"\n\033[32mPassed:\033[m {passed}, \033[31mFailed:\033[m {failed}, Took {(time.perf_counter_ns() - total_start_time)/1e9:.2f}s")

    if failed > 0:
        sys.exit(1)
