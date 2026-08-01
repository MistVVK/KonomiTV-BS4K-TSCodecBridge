# SPDX-License-Identifier: 0BSD

SHELL := /bin/bash

.DEFAULT_GOAL := build

.PHONY: benchmark build check clean compile final-release-check fixtures fixtures-check lint release-check soak soak-24h soak-600 test test-executable test-ffmpeg-integration

compile:
	bash scripts/compile.sh

lint:
	bash scripts/lint.sh

test:
	bash scripts/test.sh

fixtures:
	bash scripts/generate-fixtures.sh

fixtures-check:
	bash scripts/verify-fixture-manifest.sh

benchmark:
	bash scripts/benchmark-large-pes.sh

soak:
	bash scripts/soak.sh

build:
	bash scripts/build.sh

test-executable: build
	bash scripts/test-executable.sh

test-ffmpeg-integration:
	bash scripts/test-ffmpeg-integration.sh

release-check:
	bash scripts/release-check.sh

final-release-check:
	bash scripts/final-release-check.sh

soak-600:
	SOAK_DURATION_SECONDS=600 bash scripts/soak.sh

soak-24h:
	SOAK_MODE=av1 SOAK_DURATION_SECONDS=86400 bash scripts/soak.sh

check: compile lint test fixtures-check

clean:
	rm -f build/ts-codec-bridge.elf
	rmdir build 2>/dev/null || true
