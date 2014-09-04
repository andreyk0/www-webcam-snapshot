
default: build

%:
	cabal $@

init:
	cabal sandbox init
	cabal install yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals

deps:
	cabal install --dependencies-only

configure_profiler:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all # --ghc-option=-ddump-simpl

clean:
	cabal clean
	rm -vf www-webcam-snapshot.aux www-webcam-snapshot.hp www-webcam-snapshot.prof www-webcam-snapshot.ps

.PHONY: deps init default
