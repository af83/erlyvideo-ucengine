all:
	ERL_LIBS=.. erl -make
	(cd deps/ibrowse && make)
	cp -r deps/ibrowse/ebin/* ebin/.

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump
