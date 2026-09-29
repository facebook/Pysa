# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any

from pysa import _test_sink, _test_source


class Base:
    payload: Any

    def sinks(self) -> None:
        # `SkipModelBroadening` preserves both paths in this model.
        _test_sink(self.payload.left)
        _test_sink(self.payload.right)

    def handle(self) -> None:
        # The width limit collapses both callee paths to `self.payload`, while the
        # embedded call frames keep their original `left` and `right` paths.
        self.sinks()


class Child(Base):
    def handle(self) -> None:
        pass


class AnotherChild(Base):
    def handle(self) -> None:
        pass


def issue(base: Base) -> None:
    base.payload.left = _test_source()
    # This goes through the synthetic override model and currently produces an
    # issue whose backward trace has no roots.
    base.handle()
