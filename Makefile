.PHONY: gen-hs
gen-hs:
		hsc2hs ./app/Gpio/Ioctl.hsc -o ./app/Gpio/Ioctl.hs


.PHONY: watch
watch:
		ghciwatch --watch app --enable-eval --clear
