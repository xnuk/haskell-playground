apt-get update -y
apt-get install curl apt-transport-https -y
curl -sL 'https://keybase.io/crystal/pgp_keys.asc' | apt-key add -
echo "deb https://dist.crystal-lang.org/apt crystal main" | tee /etc/apt/sources.list.d/crystal.list
apt-get update -y
apt install crystal -y
crystal build update-stack.cr --release --static --no-debug -o update-stack
strip update-stack
