#!/bin/bash
# Exit on error
set -e

# Check for root
if [[ $EUID -ne 0 ]]; then
   echo "You must be root." 1>&2
   exit 100
fi

echo "Running as root";

# Create heed user
useradd --home-dir /etc/heed --create-home --shell /bin/false --system heed -p "useless"

echo "Created heed user";

# Remove heed user password and disable login
passwd -d heed
usrmod -s /sbin/nologin heed

cp ./confs/backend.ini /etc/heed/

# Become postgres to create heed user and import table structure

echo "Setting up postgres"
su - postgres
sudo -u postgres psql -h localhost -f ./confs/db/user.sql
sudo -u postgres psql -h localhost -f ./confs/db/tables.sql
# Add my user, hardcode everything for now
sudo -u postgres psql -h localhost -f ./confs/db/arguggi.sql
echo "PostgreSQL setup"

# Build the backend
echo "Building"
cabal new-build heed-backend

# Copy backend to to /usr/bin
echo "Copying binary to /usr/bin/"
find dist-newstyle -iname "heed-backend" -type f -exec cp {} /usr/bin/heed-backend \;

# Copy systemd service file

echo "Copying service file to /usr/lib/systemd/system/heed.service"
cp ./confs/heed.service /usr/lib/systemd/system/heed.service

chown heed:heed /etc/heed/backend.ini

# All done
echo "All done, remember to start/enable heed.service"
