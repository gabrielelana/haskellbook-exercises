FINGERDB = ./finger.db

.PHONY: start
start: start-fingerd

build: src/*.hs
	stack build

debug: build

start-debug: debug
	sudo `find . -type f -name $@ | head -1`

fingerd: build

start-fingerd: fingerd init
	sudo `find . -type f -name $@ | head -1`

fingerctl: build

init: fingerctl
	@$(eval FINGERCTL := $(shell find . -type f -name $< | head -1))
	rm -rf ${FINGERDB}
	${FINGERCTL} init

check: init
	${FINGERCTL} insert foo "/bin/bash" "/home/foo" "Foo" "555-811-325"
	${FINGERCTL} update foo 2 "/bin/bash" "/home/foo" "Foo Bar" "555-811-325"
	echo "select * from users" | sqlite3 finger.db

.PHONY: clean
clean:
	rm -rf ${FINGERDB}
	rm -rf .stack-work/
