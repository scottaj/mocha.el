CASK = cask
NPM  = npm

all: install test


# Run the project's test-suite
test: unit ecukes

unit:
	$(CASK) exec ert-runner

ecukes:
	$(CASK) exec ecukes

.PHONY: test unit ecukes


# Install dependencies needed to hack on this project
install: .cask sample-project/node_modules

.cask:
	$(CASK) --debug --verbose install

sample-project/node_modules:
	cd $(@D) && $(NPM) install


# Wipe generated and untracked files
clean:
	rm -rf .cask sample-project/node_modules test/sandbox

.PHONY: clean
