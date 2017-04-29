set -e
# allow the container to be started with `--user`
if [ "$1" = "redis-server" -a "$(${coreutils}/bin/id -u)" = "0" ]; then
  chown -R redis .
fi
exec "$@"

