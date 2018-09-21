project: out.js
	echo "#!/usr/bin/env node" > project
	cat out.js >> project
	chmod +x project

out.js: src/*.purs
	pulp build --to out.js

clean:
	rm -Rf output/ .pulp-cache/ .purs-repl out.js project

install: project
	cp -Rv project ${HOME}/bin/
