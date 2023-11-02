import subprocess
try:
    from termcolor import colored
except ModuleNotFoundError:
    def colored(string, *args, **kwargs):
        return string
from sys import argv

class TestFile:
    def __init__(self, filename, expected_code = 0, expected_output = "", expected_in_error = [], folder = "test_files/") -> None:
        self.folder = folder
        self.filename = filename
        self.timed_out = False
        self.expected_code = expected_code
        self.expected_in_error = ["ERROR"] + expected_in_error if expected_code != 0 else expected_in_error
        self.expected_output = expected_output
        self.actual = ""
        self.error_channel = ""
        self.exit_code = 0
        self.has_ran = False
        self.options = []

    def run(self):
        self.has_ran = True
        try:
            res = subprocess.run(["./glados", self.folder + self.filename ] + self.options, capture_output=True, timeout=10, stdin=subprocess.DEVNULL)
        except subprocess.TimeoutExpired as e:
            self.timed_out = True
            return
        self.actual = res.stdout.decode("utf-8")
        if len(self.actual) != 0 and (self.actual[-1] == '\n'):
            self.actual = self.actual[:-1]
        self.error_channel = res.stderr.decode("utf-8")
        if len(self.error_channel) != 0 and self.error_channel[-1] == '\n':
            self.error_channel = self.error_channel[:-1]
        self.exit_code = res.returncode

    def checkResult(self, skipGood: bool) -> bool:
        if (not self.has_ran):
            print(f"File {self.filename} has not been ran yet; skipping\n")
            return False
        if self.timed_out:
            print(colored(f"File {self.filename} has timed out\n", "light_red"))
            return False
        missing_in_error = [item for item in self.expected_in_error if item.lower() not in self.error_channel.lower()]
        if self.actual != self.expected_output or self.exit_code != self.expected_code or len(missing_in_error) != 0:
            s = f"File {self.filename}: KO"
            if self.exit_code != self.expected_code:
                s += f"\nExpected exit code {self.expected_code} but got {self.exit_code}"
            if self.actual != self.expected_output:
                s += f"\nExpected output:\n'{self.expected_output}'\nBut got:\n'{self.actual}'"
                if len(self.error_channel) != 0:
                    s += f"\nError channel was not empty: \n'{self.error_channel}'"
            if len(missing_in_error) != 0:
                s += f"\nKeywords {', '.join(missing_in_error)} in error message:\nBut got\n'{self.error_channel}'"
            print(colored(s + "\n", "light_red"))
            return False
        else:
            if not skipGood:
                print(colored(f"File {self.filename}: OK", "light_green"))
            return True

class TestLisp(TestFile):
    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs, folder="test_files/lisp/")
        self.options = ["--lisp"]


lispFileList = [
    TestLisp("okay/sujet/lambda1.scm", expected_output="#<procedure>"),
    TestLisp("okay/sujet/lambda2.scm", expected_output="3"),
    TestLisp("okay/sujet/lambda3.scm", expected_output="7"),
    TestLisp("okay/sujet/function1.scm", expected_output="7"),
    TestLisp("okay/sujet/if1.scm", expected_output="1"),
    TestLisp("okay/sujet/if2.scm", expected_output="2"),
    TestLisp("okay/sujet/if3.scm", expected_output="21"),
    TestLisp("okay/sujet/builtins1.scm", expected_output="11"),
    TestLisp("okay/sujet/builtins2.scm", expected_output="#t"),
    TestLisp("okay/sujet/builtins3.scm", expected_output="#f"),
    TestLisp("okay/sujet/builtins2.scm", expected_output="#t"),
    TestLisp("okay/sujet/factorial.scm", expected_output="3628800"),



    TestLisp("okay/factorial.txt",          expected_output="120"),
    TestLisp("okay/negative.txt",           expected_output="-1"),
    TestLisp("okay/max_int.txt",            expected_output="9223372036854775807"),
    TestLisp("okay/min_int.txt",            expected_output="-9223372036854775808"),
    TestLisp("okay/foo.txt",                expected_output="42"),
    TestLisp("okay/lazy_vars.txt",          expected_output="1"),
    TestLisp("okay/var_attribution.txt",    expected_output=""),
    TestLisp("okay/sort_list.txt",          expected_output="[1,2,3,4]\n[1,2,3,4]\n[]\n[1,1,1,1]\n[2]"),

    TestLisp("okay/basic_operations/add.txt", expected_output="5"),
    TestLisp("okay/basic_operations/sub.txt", expected_output="-1"),
    TestLisp("okay/basic_operations/mul.txt", expected_output="6"),
    TestLisp("okay/basic_operations/div.txt", expected_output="0"),
    TestLisp("okay/basic_operations/mod.txt", expected_output="2"),
    TestLisp("okay/basic_operations/pow.txt", expected_output="8"),

    TestLisp("error/div-zero.txt", expected_code=84,        expected_in_error=["division","zero"]),
    TestLisp("error/mod-zero.txt", expected_code=84,        expected_in_error=["mod","zero"]),
    TestLisp("error/unknown_var.txt", expected_code=84,     expected_in_error=["foo"]),
    TestLisp("error/missing.txt", expected_code=84,         expected_in_error=[]),
    TestLisp("error/unmatched.txt", expected_code=84,       expected_in_error=["parenthesis"]),
]

def main():
    skipGood = "--skip-good" in argv
    success_count = 0
    for file in lispFileList:
        file.run()
        if (file.checkResult(skipGood)):
            success_count += 1
    if (success_count == len(lispFileList)):
        print(colored(f"{success_count}/{success_count} test passed", "light_green"))
        return 0
    else:
        print(colored(f"{success_count}/{len(lispFileList)} test passed", "light_red"))
        return 84

if __name__ == "__main__":
    exit(main())
