import subprocess
try:
    from termcolor import colored
except ModuleNotFoundError:
    def colored(string, *args, **kwargs):
        return string
from sys import argv

class TestFile:
    folder = "test_files/"
    def __init__(self, filename, expected_code = 0, expected_output = "", expected_in_error = []) -> None:
        self.filename = filename
        self.timed_out = False
        self.expected_code = expected_code
        self.expected_in_error = ["ERROR"] + expected_in_error if expected_code != 0 else expected_in_error
        self.expected_output = expected_output
        self.actual = ""
        self.error_channel = ""
        self.exit_code = 0
        self.has_ran = False

    def run(self):
        self.has_ran = True
        try:
            res = subprocess.run(["./glados", self.folder + self.filename], capture_output=True, timeout=2, stdin=subprocess.DEVNULL)
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
        if self.actual != self.expected_output or self.exit_code != self.exit_code or len(missing_in_error) != 0:
            s = f"File {self.filename}: KO"
            if self.exit_code != self.exit_code:
                s = s + f"\nExpected exit code {self.exit_code} but got {self.expected_code}"
            if self.actual != self.expected_output:
                s = s + f"\nExpected output:\n'{self.expected_output}'\nBut got:\n'{self.actual}'"
            if len(missing_in_error) != 0:
                s = s + f"\nKeywords {', '.join(missing_in_error)} in error message:\nBut got\n'{self.error_channel}'"
            print(colored(s + "\n", "light_red"))
            return False
        else:
            if not skipGood:
                print(colored(f"File {self.filename}: OK", "light_green"))
            return True

fileList = [
    TestFile("okay/sujet/lambda1.scm", expected_output="#<procedure>"),
    TestFile("okay/sujet/lambda2.scm", expected_output="3"),
    TestFile("okay/sujet/lambda3.scm", expected_output="7"),
    TestFile("okay/sujet/function1.scm", expected_output="7"),
    TestFile("okay/sujet/if1.scm", expected_output="1"),
    TestFile("okay/sujet/if2.scm", expected_output="2"),
    TestFile("okay/sujet/if3.scm", expected_output="21"),
    TestFile("okay/sujet/builtins1.scm", expected_output="11"),
    TestFile("okay/sujet/builtins2.scm", expected_output="#t"),
    TestFile("okay/sujet/builtins3.scm", expected_output="#f"),
    TestFile("okay/sujet/builtins2.scm", expected_output="#t"),
    TestFile("okay/sujet/factorial.scm", expected_output="3628800"),



    TestFile("okay/factorial.txt",          expected_output="120"),
    TestFile("okay/negative.txt",           expected_output="-1"),
    TestFile("okay/max_int.txt",            expected_output="9223372036854775807"),
    TestFile("okay/min_int.txt",            expected_output="-9223372036854775808"),
    TestFile("okay/foo.txt",                expected_output="42"),
    TestFile("okay/lazy_vars.txt",          expected_output="1"),
    TestFile("okay/var_attribution.txt",    expected_output=""),
    TestFile("okay/sort_list.txt",          expected_output="[1,2,3,4]\n[1,2,3,4]\n[]\n[1,1,1,1]\n[2]"),

    TestFile("okay/basic_operations/add.txt", expected_output="5"),
    TestFile("okay/basic_operations/sub.txt", expected_output="-1"),
    TestFile("okay/basic_operations/mul.txt", expected_output="6"),
    TestFile("okay/basic_operations/div.txt", expected_output="0"),
    TestFile("okay/basic_operations/mod.txt", expected_output="2"),
    TestFile("okay/basic_operations/pow.txt", expected_output="8"),

    TestFile("error/div-zero.txt", expected_code=84,        expected_in_error=["division","zero"]),
    TestFile("error/mod-zero.txt", expected_code=84,        expected_in_error=["mod","zero"]),
    TestFile("error/unknown_var.txt", expected_code=84,     expected_in_error=["foo"]),
    TestFile("error/missing.txt", expected_code=84,         expected_in_error=[]),
    TestFile("error/unmatched.txt", expected_code=84,       expected_in_error=["parenthesis"]),
    TestFile("error/file_not_real.txt", expected_code=84,   expected_in_error=["open", "file"]),
]

def main():
    skipGood = "--skip-good" in argv
    success_count = 0
    for file in fileList:
        file.run()
        if (file.checkResult(skipGood)):
            success_count += 1
    if (success_count == len(fileList)):
        print(colored(f"{success_count}/{success_count} test passed", "light_green"))
    else:
        print(colored(f"{success_count}/{len(fileList)} test passed", "light_red"))

if __name__ == "__main__":
    main()
