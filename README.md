clustercute
===========
It's cute, but it's wrong
-------------------------

Simple password based execute on multiple SSH hosts.
If you need this, you know you should have opted for something like Salt, Chef Puppet, or the likes.
However, it was still fun to write, so there you go.

    clustercute <file with hosts and commands>

Usage
-----
The first (and only) argument to the program should be a file containing host and command to run on that host per line:

    localhost echo Locally here
    127.0.0.1 date
    example.com date

The first word is considered the host, the rest is considered the command.

Then execute `clustercute` with that file as the one and only argument.
It will prompt for username and password which is
then used to log into every host and execute the commands given in the file.

Everything is logged with timestamps into `clustercute.log`.
