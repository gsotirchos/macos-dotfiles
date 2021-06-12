#!/usr/bin/env bash

CROP=true
SPLIT=true
CROP_SIZE_X=256
CROP_SIZE_Y=192
CROP_OFFSET_X=112
CROP_OFFSET_Y=233
REPAGE_SIZE_X=257
REPAGE_SIZE_Y=193
REPAGE_OFFSET_X=16
REPAGE_OFFSET_Y=8
TILE_SIZE_X=32
TILE_SIZE_Y=32

CROP_MASK="${CROP_SIZE_X}x${CROP_SIZE_Y}\
+${CROP_OFFSET_X}+${CROP_OFFSET_Y}"
REPAGE_MASK=("${REPAGE_SIZE_X}x${REPAGE_SIZE_Y}\
+${REPAGE_OFFSET_X}+${REPAGE_OFFSET_Y}")
TILE_SIZE="${TILE_SIZE_X}x${TILE_SIZE_Y}"

if [[ "${CROP}" = true ]]; then
    mkdir "cropped"
fi

if [[ "${SPLIT}" = true ]]; then
    mkdir "tiles"
fi

file=""

for file in *.png; do
    if [[ "${CROP}" = true ]]; then
        # crop
        cropped_file=cropped/"cropped_${file}"
        convert -crop "${CROP_MASK}" "${file}" "${cropped_file}"
        echo "cropped: ${file}";
    else
        cropped_file="${file}"
        echo "skipped cropping: ${file}"
    fi

    if [[ "${SPLIT}" = true ]]; then
        # split to tiles
        convert -repage "${REPAGE_MASK}" -crop "${TILE_SIZE}" +repage \
            "${cropped_file}" tiles/"tile_%03d_${file}"
        echo "split tiles from: ${file}";
    fi
done

if [[ "${file}" = "" ]]; then
    echo "No files found!"
fi