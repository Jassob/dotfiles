#!/usr/bin/env sh -eo

ENV_DIR="${HOME}/.cache"
ENV_FILE="${ENV_DIR}/disturb"
DO_NOT_DISTURB=$(cat "${ENV_FILE}")

if [ ! -d ${ENV_FILE} ]; then
    mkdir -p "${ENV_DIR}"
    echo "false" > "${ENV_FILE}"
fi

if [ ${DO_NOT_DISTURB} = "true" ]; then
	echo ""
elif [ ${DO_NOT_DISTURB} = "false" ]; then
	echo ""
else
	echo "error: DO_NOT_DISTURB is not set to either true or false"
fi
