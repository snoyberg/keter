#!/usr/bin/env bash

set -xe

/usr/bin/env sleep 10

/usr/bin/env nc -l "$PORT"
