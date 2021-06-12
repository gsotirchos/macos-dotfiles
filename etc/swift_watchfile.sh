#!/usr/bin/env bash

#
# ~/bin/swift-watch
#

# Usage
USAGE="usage: `basename $0` [project directory location]"
[[ -z "$1" ]]      && echo -e "${USAGE}" && exit 1
[[ -n "$2" ]]      && echo -e "${USAGE}" && exit 1
[[ "$1" == "-h" ]] && echo -e "${USAGE}" && exit 0

# store project name
PROJECT=$(grealpath $1)
PROJECT_NAME=$(basename ${PROJECT})

cd "${PROJECT}"

# function to compile project and execute the binary
function build_and_execute() {
    # kill internal processes on exit
    trap 'jobs -p | xargs kill' EXIT

    # attempt to build project
    clear # clear screen
    echo -e "--- Compiling ---"
    swift build &&
        echo -e "-----------------\n"

    # execute the built binary
    echo -e "--- Executing ---"
    "${PROJECT}/.build/debug/${PROJECT_NAME}" &&
        echo -e "-----------------\n"

    echo "Watching \"${PROJECT_NAME}\"..."
}
export -f build_and_execute

function watch_file () {
    # watch file for changes and forcefully build on detect
    fswatch "${PROJECT}/Sources" | while read > /dev/null; do
        kill ${task_pid} &> /dev/null
        wait ${task_pid} &> /dev/null
        echo "Rebuilding..."

        # wait for file writes to finish
        files_changed=true
        while [[ ${files_changed} = true ]]; do
            files_changed=false
            [[ -n \
                $(gtimeout 1 fswatch "${PROJECT}/.build" -1 -l 0.1)
            ]] &&
                files_changed=true
        done

        # build and execute the binary
        build_and_execute &
            task_pid=$!
    done
}
export -f watch_file

function main() {
    # exit script on ctrl-c
    trap 'echo ": Interrupted" && exit 1' INT

    # compile once first
    build_and_execute &
        task_pid=$!

    watch_file
}

main