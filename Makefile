all: clean target/splunge

.PHONY: clean
clean:
	rm -rf target

target/splunge-standalone.jar:
	clojure -T:build uber

target/splunge: target/splunge-standalone.jar
	native-image \
		--no-fallback \
		--allow-incomplete-classpath \
		-jar target/splunge-standalone.jar \
		-H:Path=target \
		-H:Name=splunge
	[ "$$(echo test | ./target/splunge 'search test')" = "test" ]

.PHONY: test
test:
	clojure -X:test
