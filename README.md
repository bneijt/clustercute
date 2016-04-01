clustercute
===========
It's cute, but it's wrong
-------------------------

Simple password based execute on multiple SSH hosts. If you need this, you know you should have opted for something like Salt, Chef Puppet, or the likes.

    clustercute <file with hosts and commands>

First it will ask you for a username and password (twice, to make sure you typed it in correctly).

Usage
-----
The first (and only) argument to the program should be a file containing host and command to run on that host per line:

    localhost echo Locally here
    127.0.0.1 date
    example.com date

The first word is considered the host, the rest is considered the command.
