.PHONY: default
default: build

.PHONY: build
build:
	jbuilder build --dev

.PHONY: clean
clean:
	jbuilder clean
