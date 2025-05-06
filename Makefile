##
# Copyright 2025 Charles Y. Choi
#

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
INSTALL_DIR=$(HOME)/bin
EXEC_NAME=foo

.PHONY: checkout-development
checkout-development:
	git checkout development
	git branch --set-upstream-to=origin/development development
	git fetch origin --prune
	git pull

.PHONY: checkout-main
checkout-main:
	git checkout main
	git branch --set-upstream-to=origin/main main
	git fetch origin --prune
	git pull

.PHONY: sync-development-with-main
sync-development-with-main: checkout-main checkout-development
	git merge main

.PHONY: create-merge-development-branch
create-merge-development-branch: checkout-development
	git checkout -b merge-development-to-main-$(TIMESTAMP)
	git push --set-upstream origin merge-development-to-main-$(TIMESTAMP)

## Create GitHub pull request for development
.PHONY: create-pr
create-pr:
	gh pr create --base development --fill

.PHONY: create-patch-pr
create-patch-pr:
	gh pr create --base main --fill

.PHONY: create-release-pr
create-release-pr: create-merge-development-branch
	gh pr create --base main \
--title "Merge development to main $(TIMESTAMP)" \
--fill-verbose
