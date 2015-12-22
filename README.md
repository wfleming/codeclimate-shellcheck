Please note, this engine is not currently available on the CodeClimate platform.

# Code Climate ShellCheck Engine

[![Build Status](https://travis-ci.org/filib/codeclimate-shellcheck.svg?branch=master)](https://travis-ci.org/filib/codeclimate-shellcheck)

`codeclimate-shellcheck` is a Code Climate engine that wraps
[ShellCheck](http://www.shellcheck.net/). You can run it on your
command line using the Code Climate CLI, or on our hosted analysis
platform.

`ShellCheck` is a static analysis tool for shell scripts.

### Installation

1. If you haven't already, [install the Code Climate CLI](https://github.com/codeclimate/codeclimate).
2. Run `codeclimate engines:enable shellcheck`. This command both installs the engine and enables it in your `.codeclimate.yml` file.
3. You're ready to analyze! Browse into your project's folder and run `codeclimate analyze`.

### Need help?

For help with `ShellCheck`,
[check out their documentation](http://www.shellcheck.net/).

If you're running into a Code Climate issue, first look over this
project's
[GitHub Issues](https://github.com/filib/codeclimate-shellcheck),
as your question may have already been covered. If not,
[go ahead and open a support ticket with us](https://codeclimate.com/help).
