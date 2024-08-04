#!/bin/sh

## Script that does a full system backup on my main Garuda workstation,
## including its Samba share volume.  Destination is a USB flash drive that is
## manually inserted prior to running this script.
##
## Usage: Run with sudo.

# Identify the USB flash drive by its UUID.
UUID="2c269d4d-8312-4946-98df-b8122532c541"

# Exit if the /data directory doesn't exist, in order to not make a mess.
if [ ! -d "/data" ]; then
    echo "/data directory doesn't exist!"
    exit 1
fi

# Exit if the USB flash drive isn't present.
if ! blkid -U $UUID; then
    echo "Backup target volume doesn't exist!"
    exit 1
fi

# Mount the USB flash drive.
mount -U $UUID /mnt/usbstick

# Only proceed if mount was successful, to avoid duping the full FS locally.
mount | grep -q "^/dev/sdb1 on /mnt/usbstick"
if [ $? -eq 0 ]; then
    echo "USB volume is mounted"
else
    echo "USB volume is not mounted!"
    exit 1
fi

# Normalize permissions on the /data directory.
cd /data
find . -type d -exec chown bm3719:bm3719 {} \;
find . -type f -exec chown bm3719:bm3719 {} \;
find . -type d -exec chmod 2777 {} \;
find . -type f -exec chmod 2666 {} \;
cd -

# Use rsync to copy and sync the entire filesystem to the flash drive.
rsync -aAHXv --exclude '/dev' --exclude '/proc' --exclude '/sys' --exclude '/tmp' --exclude '/run' --exclude '/mnt' --exclude '/var' --exclude '/.snapshots' / /mnt/usbstick

# Unmount the USB flash drive
umount /mnt/usbstick
