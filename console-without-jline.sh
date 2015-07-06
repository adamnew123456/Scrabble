#!/bin/sh
exec sbt -Djline.terminal=jline.UnsupportedTerminal console
