PROJECT = durga

# dependencies

DEPS = gen_cowboy cowboy cowlib cowboy_compiled_router annex_marshal_msgpack

SHELL_DEPS = rl

dep_gen_cowboy = git https://github.com/camshaft/gen_cowboy.git master
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_cowlib = git https://github.com/ninenines/cowlib 1.0.1
dep_cowboy_compiled_router = git https://github.com/camshaft/cowboy_compiled_router master
dep_rl = git https://github.com/camshaft/rl master
dep_annex_marshal_msgpack = git https://github.com/annexrpc/annex-marshal-msgpack.git master

include erlang.mk

repl: all bin/start
	@bin/start durga -s rl make

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
