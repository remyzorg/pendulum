.PHONY: default
default: build

.PHONY: test
test: build
	jbuilder runtest --dev -j 7 tests/ppx
	@rm -r _build/default/tests/ppx

.PHONY: testjs
testjs: build
	jbuilder runtest --dev -j 7 tests/ppx_js
	@rm -r _build/default/tests/ppx_js

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

