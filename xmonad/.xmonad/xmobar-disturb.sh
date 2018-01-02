#! env bash

ENV_FILE="${HOME}/.local/share/env/disturb"
DO_NOT_DISTURB=$(cat "${ENV_FILE}")

if [[ ${DO_NOT_DISTURB} == "true" ]]; then
	echo ""
elif [[ ${DO_NOT_DISTURB} == "false" ]]; then
	echo ""
else
	echo "error: DO_NOT_DISTURB is not set to either true or false"
fi
