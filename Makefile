doozer.exe: qlot-manifest.txt
	buildapp --manifest-file "$<" --asdf-path . --load-system probuild --entry probuild::main --output $@

qlot-manifest.txt: qlot-install-timestamp
	sbcl --disable-debugger \
	     --eval '(ql:quickload :qlot)' \
	     --eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
	     --eval '(qlot:with-local-quicklisp (:probuild) (ql:write-asdf-manifest-file #P"$@" :exclude-local-projects t))' \
	     --eval '(sb-ext:quit)'

qlot-install-timestamp: qlfile.lock
	sbcl --disable-debugger \
	     --eval '(ql:quickload :qlot)' \
	     --eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
	     --eval '(qlot:install :probuild)' \
	     --eval '(sb-ext:quit)'
	date > "$@"

