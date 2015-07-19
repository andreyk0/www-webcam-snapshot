build:
	stack build

clean:
	stack clean
	32bit-haskell-platform stack clean

tags:
	hasktags --ctags --extendedctag  . .sources/*

ubilinux-build:
	ubilinux-haskell-platform stack build

32bit-build:
	32bit-haskell-platform stack build

install: ubilinux-build
	rsync -av --delete \
		.stack-work/install/i386-linux/lts-2.18/7.8.4/bin/www-webcam-snapshot \
		edison:/www/www-webcam-snapshot/bin/

.PHONY: \
	clean \
	default \
	build \
	32bit-build \
	deps \
	init \
	tags \

