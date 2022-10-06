#!/bin/sh
# Command to transform Markdown to LaTeX pdf using programme pandoc
pandoc -s -f markdown -V linkcolor:blue -t latex Contribution.md -o Contribution.pdf
