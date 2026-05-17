.PHONY: gen-hs
gen-hs:
		hsc2hs ./app/GPIO/Raw.hsc -o ./app/GPIO/Raw.hs
