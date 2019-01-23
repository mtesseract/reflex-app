#!/bin/sh

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <remote ssh>" >&2
    exit 1
fi

REMOTE="$1"
echo "**** Deploying to $1 ****"

nix-build --attr system deploy.nix
artifact=$(readlink result)
nix-copy-closure $REMOTE $artifact
ssh $REMOTE "${artifact}/bin/switch-to-configuration" switch
