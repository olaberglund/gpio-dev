.PHONY: gen-hs
gen-hs:
		hsc2hs ./app/GPIO/Raw.hsc -o ./app/GPIO/Raw.hs


.PHONY: watch
watch:
		ghciwatch --watch app --enable-eval --clear
