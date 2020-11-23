start-scotty:
	stack run & echo $$! > .pid

stop-scotty: .pid
	kill `cat .pid` && rm -f .pid

start-redis:
	docker run --rm -p 127.0.0.1:6379:6379 --name redis -d redis

stop-redis:
	docker stop redis

start: start-redis start-scotty

stop: stop-scotty stop-redis

.PHONY: start-redis stop-redis start-scotty
