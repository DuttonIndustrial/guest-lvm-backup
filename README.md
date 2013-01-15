guest-lvm-backup
================

Allows full backups of virtual guest operating systems that live upon lvm partitions across slow links.

The Problem:
    You have a virtual machine host isolated behind a slow internet uplink, 
and you need to make frequent backups offsite.

How guest-lvm-backup solves it:
    guest-lvm-backup can upload an entire disk image nightly over a slow link. 
It does this by comparing the previous backup image, to the local image
and upload the differences. Thus making full nightly
backups of a 100GB image over 1MB/s link easy to do.

guest-lvm-backup is coded in racket a scheme like language and relies upon
rdiff, which is built upon the same library that rsync uses.



