public/js/bundle.js: ../onbrowser.ml
	mkdir -p public
	mkdir -p public/js
	./node_modules/bs-platform/bin/bsc.exe -c -I src/ -bs-main ../onbrowser.ml -bs-package-name bs-mcrd -bs-package-output public/js
	browserify public/js/onbrowser.js -o public/js/bundle.js

