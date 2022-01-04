all: native-image

.PHONY: uber
uber:
	clojure -T:build uber

.PHONY: native-image
native-image: uber
	native-image \
		--no-fallback \
		--allow-incomplete-classpath \
		-jar target/splunge-standalone.jar \
		-H:Name=target/splunge
	[ "$$(echo test | ./target/splunge 'search test')" = "test" ]

.PHONY: test
test:
	clojure -X:test
