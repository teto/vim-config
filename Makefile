ui:
	pyuic5 untitled.ui -o configuratorUI.py

csv:
	lua convert_to_csv.lua > options.csv

.PHONY: ui

