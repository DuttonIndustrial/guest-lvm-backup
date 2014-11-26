To install systemwide:
	1. You need racket installed on your system. Go here: http://racket-lang.org/
	2. clone this repo to somewhere on your system. I recommend /usr/local/guest-lvm-backup
	3. raco link -i /usr/local/guest-lvm-backup
	4. raco setup -l guest-lvm-backup (this will install commands guest-lvm-backup and guest-lvm-tape to /usr/local/bin

	
Tested with:
	ubuntu 11.10 x86-64
	ubuntu 12.10 x86-64

guest-lvm-backup
================

Allows full backups of virtual guest operating systems that live upon lvm partitions across slow links.

The Problem:
    You have a virtual machine host isolated behind a slow internet uplink, 
and you need to make frequent backups offsite.

How guest-lvm-backup solves it:
    guest-lvm-backup can upload an entire disk image nightly over a slow link. 
It does this by comparing the previous remote backup image, to the local image and uploading the differences. Thus making full nightly backups of a 100GB image over 1MB/s link easy to do.


What it does:
	guest-lvm-backup shutsdown the specified guest os
	snapshots its lvm volume
	restarts the guest
	creates a gzipped copy of the lvm volume to [user]@remotehost:/home/[user]/backups/[name-of-vm-host]-[name-of-guest]-backup-[time-in-seconds-of-snapshot].gz
	removes the snapshot
	

guest-lvm-backup is coded in racket a scheme like language and relies upon
rdiff, which is built upon the same library that rsync uses.


guest-lvm-tape
===============

Similar to guest-lvm-backup, but blindly copies all of the data to a tape instead of a remote backup location. guest-lvm-tape does no rsync magic. 

Currently it copies 2 files. The first file contains information about the backup. It is a small text file.
The second file is and entire gzipped copy of the logical volume.









