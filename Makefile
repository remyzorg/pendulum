.PHONY: default
default: build

.PHONY: test
test: build
	jbuilder runtest --dev -j 1 --no-buffer

.PHONY: build
build:
	jbuilder build --dev

.PHONY: install
install: build
	jbuilder install

.PHONY: uninstall
uninstall:
	jbuilder uninstall

.PHONY: clean
clean:
	jbuilder clean

