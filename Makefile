# Invoke `make all` to build, `make clean` to clean up, etc.
# If you don't want to use make, you can run the commands directly
# such as `dune build @install`

# Build
all:
	dune build @install

.PHONY: clean all test install

install: all
	dune install

# Build and run tests
test:
	dune runtest

# Clean up
clean:
	dune clean
