# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import unittest
from pathlib import Path
from unittest.mock import call, MagicMock, patch

from .. import setup


class FullSetupTest(unittest.TestCase):
    @patch.object(setup, "_run_command")
    def test_buck_dev_build(self, run_command: MagicMock) -> None:
        run_command.side_effect = [
            "a" * 40,
            json.dumps({"main": "/tmp/main"}),
            json.dumps({"buck_main": "/tmp/buck_main"}),
        ]
        pyre_directory = Path("/repo/fbcode/tools/pyre")

        binaries = setup.full_setup(
            pyre_directory,
            build_system=setup.BuildSystem.BUCK,
            build_type=setup.BuildType.FACEBOOK,
        )

        self.assertEqual(binaries.pyre_binary, Path("/tmp/main"))
        self.assertEqual(binaries.buck_main_binary, Path("/tmp/buck_main"))
        self.assertEqual(
            run_command.call_args_list,
            [
                call(
                    ["hg", "log", "-r", ".", "-T", "{node}"],
                    current_working_directory=pyre_directory,
                ),
                call(
                    [
                        "buck2",
                        "build",
                        "--show-full-json-output",
                        "@fbcode//mode/dev",
                        "-c",
                        f"pyre.version={'a' * 40}",
                        "fbcode//tools/pyre/source:main",
                    ],
                    current_working_directory=pyre_directory,
                    add_environment_variables=None,
                ),
                call(
                    [
                        "buck2",
                        "build",
                        "--show-full-json-output",
                        "@fbcode//mode/dev",
                        "-c",
                        f"pyre.version={'a' * 40}",
                        "fbcode//tools/pyre/source:buck_main",
                    ],
                    current_working_directory=pyre_directory,
                    add_environment_variables=None,
                ),
            ],
        )

    @patch.object(setup.os, "cpu_count", return_value=64)
    @patch.object(setup, "_run_command")
    def test_buck_release_tests_cap_concurrency(
        self, run_command: MagicMock, _cpu_count: MagicMock
    ) -> None:
        run_command.side_effect = [
            "b" * 40,
            json.dumps({"main": "/tmp/main"}),
            json.dumps({"buck_main": "/tmp/buck_main"}),
            "",
        ]
        pyre_directory = Path("/repo/fbcode/tools/pyre")

        setup.full_setup(
            pyre_directory,
            build_system=setup.BuildSystem.BUCK,
            build_type=setup.BuildType.FACEBOOK,
            release=True,
            run_tests=True,
        )

        self.assertEqual(
            run_command.call_args_list[-1],
            call(
                [
                    "buck2",
                    "test",
                    "-j",
                    "32",
                    "@fbcode//mode/opt",
                    "-c",
                    f"pyre.version={'b' * 40}",
                    "fbcode//tools/pyre/source/...",
                ],
                current_working_directory=pyre_directory,
                add_environment_variables=None,
            ),
        )

    @patch.object(setup.os, "cpu_count", return_value=64)
    @patch.object(setup, "_run_command")
    def test_buck_external_build_and_tests(
        self, run_command: MagicMock, _cpu_count: MagicMock
    ) -> None:
        run_command.side_effect = [
            "c" * 40,
            json.dumps({"main": "/tmp/main"}),
            json.dumps({"buck_main": "/tmp/buck_main"}),
            "",
        ]
        pyre_directory = Path("/repo/fbcode/tools/pyre")

        binaries = setup.full_setup(
            pyre_directory,
            build_system=setup.BuildSystem.BUCK,
            build_type=setup.BuildType.EXTERNAL,
            run_tests=True,
        )

        self.assertEqual(binaries.pyre_binary, Path("/tmp/main"))
        self.assertEqual(binaries.buck_main_binary, Path("/tmp/buck_main"))
        self.assertEqual(
            run_command.call_args_list,
            [
                call(
                    ["hg", "log", "-r", ".", "-T", "{node}"],
                    current_working_directory=pyre_directory,
                ),
                call(
                    [
                        "buck2",
                        "build",
                        "--show-full-json-output",
                        "@fbcode//mode/dev",
                        "-c",
                        f"pyre.version={'c' * 40}",
                        "-m",
                        "fbcode//tools/pyre/source:build_type[external]",
                        "fbcode//tools/pyre/source:main",
                    ],
                    current_working_directory=pyre_directory,
                    add_environment_variables=None,
                ),
                call(
                    [
                        "buck2",
                        "build",
                        "--show-full-json-output",
                        "@fbcode//mode/dev",
                        "-c",
                        f"pyre.version={'c' * 40}",
                        "-m",
                        "fbcode//tools/pyre/source:build_type[external]",
                        "fbcode//tools/pyre/source:buck_main",
                    ],
                    current_working_directory=pyre_directory,
                    add_environment_variables=None,
                ),
                call(
                    [
                        "buck2",
                        "test",
                        "-j",
                        "32",
                        "@fbcode//mode/dev",
                        "-c",
                        f"pyre.version={'c' * 40}",
                        "-m",
                        "fbcode//tools/pyre/source:build_type[external]",
                        "fbcode//tools/pyre/source/...",
                    ],
                    current_working_directory=pyre_directory,
                    add_environment_variables=None,
                ),
            ],
        )

    def test_opam_is_default_and_requires_root_and_version(self) -> None:
        with self.assertRaisesRegex(ValueError, "OPAM root and version"):
            setup.full_setup(
                Path("/repo/pyre"),
                build_type=setup.BuildType.EXTERNAL,
            )
