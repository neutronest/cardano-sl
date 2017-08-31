#!/usr/bin/env bash
set -euo pipefail

echo "Launch single node and connect it to 'staging' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
readonly SYSTEM_START_TIME=1503504180

printf "wallet:
    relays:
        [
            [
                { host: cardano-node-0.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-1.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-2.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-3.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-4.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-5.aws.iohkdev.io, port: 3000 },
                { host: cardano-node-6.aws.iohkdev.io, port: 3000 }
            ]
        ]
    valency: 3
    fallbacks: 2" > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-node-simple           \
    --no-ntp                                \
    --topology="${TMP_TOPOLOGY_YAML}"       \
    --system-start="${SYSTEM_START_TIME}"
