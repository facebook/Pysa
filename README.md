[![tests](https://github.com/facebook/Pysa/actions/workflows/pysa.yml/badge.svg)](https://github.com/facebook/Pysa/actions/workflows/pysa.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<p align="center">
  <img src="https://raw.githubusercontent.com/facebook/Pysa/main/logo.png">
</p>

Pysa is a security-focused static analysis tool for Python that tracks data flows to find security and privacy issues — for example, user-controlled input reaching a dangerous sink such as remote code execution or SQL injection. Pysa can analyze codebases with millions of lines of code. Refer to our [documentation](https://pyre-check.org/docs/pysa-basics) to get started.

Pysa relies on type information from [Pyrefly](https://pyrefly.org/), Meta's performant Python type checker.

Pysa is also available on the [GitHub Marketplace as a GitHub Action](https://github.com/marketplace/actions/pysa-action).

## Installation
Pysa requires Python 3.9 or later. Install it with pip:
```bash
$ pip install pyre-check
```
Pysa is currently distributed as part of the `pyre-check` package, since it was historically bundled with [Pyre](https://pyre-check.org/), Meta's (deprecated) type checker. In the future, Pysa will ship as its own PyPI package.

## Running Pysa
Pysa relies on type information from [Pyrefly](https://pyrefly.org/). Before running Pysa, make sure Pyrefly can successfully check your code:
```bash
$ pyrefly check
```
Once Pyrefly runs cleanly, run Pysa from your project directory to find security and privacy issues:
```bash
$ pyre analyze
```
Pysa uses models to identify sources of taint (where untrusted data enters) and sinks (dangerous operations). For details on configuring Pysa, writing models, and interpreting results, see the [Pysa documentation](https://pyre-check.org/docs/pysa-basics).

## Join the Pysa community

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to help out.

## License

Pysa is licensed under the MIT license.
