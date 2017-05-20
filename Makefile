all:
	stack build --install-ghc

install:
	stack install --install-ghc
	sudo chown root:root ~/.local/bin/xleds
	sudo chmod u+s ~/.local/bin/xleds

clean:
	rm -rf .stack-work
