.PHONY: dev prod

dev:
	lein clean && lein with-profile +macos,+dev trampoline run

uberjar:
	lein clean && lein uberjar

