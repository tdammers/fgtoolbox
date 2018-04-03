#!/bin/bash
function watch_compile() {
    stack install --file-watch
}

function watch_serve() {
    while (true)
    do
        fgtoolbox serve &
        inotifywait "$(which fgtoolbox)"
        killall fgtoolbox
        sleep 1
    done
}

watch_serve &
watch_compile
