all:
	stack build

install:
	stack install
	sudo chown root:root ~/.local/bin/xleds
	sudo chmod u+s ~/.local/bin/xleds

clean:
	rm -rf .stack-work
