#!/usr/bin/env sh -eo

ENV_DIR="${HOME}/.cache"
ENV_FILE="${ENV_DIR}/awake"

if [ ! -e "${ENV_FILE}" ]; then
    mkdir -p "${ENV_DIR}"
    echo "false" > "${ENV_FILE}"
fi

STAY_AWAKE=$(cat "${ENV_FILE}")

if [ "${STAY_AWAKE}" = "true" ]; then
    echo "<fn=3></fn>"
elif [ "${STAY_AWAKE}" = "false" ]; then
    echo "<fn=2></fn>"
fi
