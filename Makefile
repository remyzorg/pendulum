.PHONY: default
default: build

.PHONY: test
test: build
	jbuilder runtest --dev -j 1 --no-buffer

.PHONY: build
build:
	jbuilder build --dev

.PHONY: clean
clean:
	jbuilder clean

