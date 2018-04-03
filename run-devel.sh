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

function watch_js() {
    cd client
    watchify src/main.js -o js/main.js
}

watch_js &
watch_serve &
watch_compile
