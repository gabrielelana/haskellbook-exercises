.PHONY: start
start: fingerd

build: src/*.hs
	stack build

debug: src/Debug.hs
	stack build && sudo `find . -type f -name $@ | head -1`

fingerd: src/Main.hs
	stack build && sudo `find . -type f -name $@ | head -1`
