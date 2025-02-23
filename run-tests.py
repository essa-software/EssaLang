import argparse as ap
import datetime as dt
import os
import subprocess as sp
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

parser = ap.ArgumentParser("run-tests")
parser.add_argument(
    "--update",
    help="update expectations for matching tests with actual outputs",
)
parser.add_argument(
    "--update-all",
    help="update expectations for all tests with actual outputs",
    action="store_true",
)
args = parser.parse_args()

ROOT = Path(__file__).parent

TESTS_DIR = os.path.normpath(Path(ROOT) / "tests")
COMPILER_PATH = os.path.normpath(Path(ROOT) / "rust/target/debug/elc")

BINARY_DIR = Path(ROOT) / "build"
os.chdir(BINARY_DIR)


def load_expectation(path: str):
    try:
        with Path(path).open("rb") as f:
            return f.read()
    except FileNotFoundError:
        return ""


failed = 0
passed = 0


def log_print(*args: str):
    sys.stdout.write(" ".join(map(str, args)) + "\n")


def fail(test_path: Path, cause: str):
    test_name = test_path.relative_to(TESTS_DIR)
    global failed  # noqa: PLW0603 (FIXME)
    failed += 1
    log_print(f"\033[41m FAIL \033[0;34m {test_name}\033[m: {cause}")


def pass_(test_path: Path, compiler_time: float):
    test_name = test_path.relative_to(TESTS_DIR)
    global passed  # noqa: PLW0603 (FIXME)
    passed += 1
    log_print(
        f"\033[42m PASS \033[0;34m {test_name} \033[30m"
        f"({compiler_time / 1e9:.2f}s)\033[m",
    )


total_start_time = time.perf_counter_ns()


def get_expected_paths(test_path: Path):
    # Compile error (stdout)
    expected_compile_out_path = TESTS_DIR / test_path.with_suffix(
        test_path.suffix + ".cerr",
    )
    # Runtime stdout
    expected_out_path = TESTS_DIR / test_path.with_suffix(test_path.suffix + ".out")
    # Runtime stderr (e.g panics)
    expected_err_path = TESTS_DIR / test_path.with_suffix(test_path.suffix + ".err")
    return expected_compile_out_path, expected_out_path, expected_err_path


def run_compiler(test_path: Path, env_dir: Path):
    proc = sp.Popen(  # noqa: S603
        [COMPILER_PATH, str(test_path), "--machine-readable-errors"],
        stdout=sp.PIPE,
        stderr=sp.PIPE,
        cwd=env_dir,
    )
    compile_out, compile_err = proc.communicate()
    ret = proc.returncode

    return compile_out, compile_err, ret


def run_runtime(env_dir: Path):
    proc = sp.Popen(  # noqa: S603
        [env_dir / "build" / "out"],
        stdout=sp.PIPE,
        stderr=sp.PIPE,
        cwd=env_dir,
    )
    out, err = proc.communicate()
    ret = proc.returncode

    return out, err, ret


def run_test(test_path: Path, env_dir_id: int):
    env_dir = Path(BINARY_DIR) / str(env_dir_id)
    Path.mkdir(env_dir, parents=True, exist_ok=True)

    expected_compile_out_path, expected_out_path, expected_err_path = (
        get_expected_paths(test_path)
    )
    expected_compile_out = load_expectation(expected_compile_out_path)
    expected_out = load_expectation(expected_out_path)
    expected_err = load_expectation(expected_err_path)

    if (
        len(expected_compile_out) == 0
        and len(expected_out) == 0
        and len(expected_err) == 0
    ):
        return

    start_time = time.perf_counter_ns()

    # Compiler
    compile_out, compile_err, ret = run_compiler(test_path, env_dir)
    compiler_time = time.perf_counter_ns() - start_time

    if ret != 0:
        if len(expected_compile_out) > 0:
            if compile_out != expected_compile_out:
                fail(test_path, "got different compiler error than expected")
                return

            pass_(test_path, compiler_time)
            return

        fail(test_path, "expected success but got compiler error")
        return

    if len(expected_compile_out) > 0:
        fail(test_path, "expected compiler error but got success")
        return

    # Runtime
    out, err, ret = run_runtime(env_dir)

    if ret != 0:
        if len(expected_err) > 0:
            if err != expected_err:
                fail(test_path, "got different runtime error message than expected")
                return

            pass_(test_path, compiler_time)
            return

        fail(test_path, "expected success but got runtime error")
        return

    if len(expected_err) > 0:
        fail(test_path, "expected runtime error but got success")
        return

    pass_(test_path, compiler_time)


def update_test(test_path: Path, env_dir_id: int):
    cout_path, out_path, err_path = get_expected_paths(test_path)

    env_dir = Path(BINARY_DIR) / str(env_dir_id)
    Path.mkdir(env_dir, parents=True, exist_ok=True)

    # compiler
    compile_out, compile_err, ret = run_compiler(test_path, env_dir)
    if ret != 0:
        with Path(cout_path).open("wb") as f:
            f.write(compile_out)
        return

    # runtime
    out, err, ret = run_runtime(env_dir)
    if len(out) > 0:
        with Path(out_path).open("wb") as f:
            f.write(out)
    if len(err) > 0:
        with Path(err_path).open("wb") as f:
            f.write(err)


if args.update_all:
    for idx, test_path in enumerate(Path(TESTS_DIR).rglob("*.esl")):
        update_test(test_path, idx)
elif args.update:
    update_test(TESTS_DIR / Path(args.update), 0)
else:
    MAX_THREADS = 1
    with ThreadPoolExecutor(max_workers=MAX_THREADS) as pool:
        for idx, test_path in enumerate(Path(TESTS_DIR).rglob("*.esl")):
            pool.submit(run_test, test_path, idx)

    log_print(
        f"\n\033[32mPassed:\033[m {passed}, \033[31mFailed:\033[m {failed}, "
        f"Took {(time.perf_counter_ns() - total_start_time) / 1e9:.2f}s",
    )

    if failed > 0:
        sys.exit(1)
