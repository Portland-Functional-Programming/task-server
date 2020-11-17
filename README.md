# Task Manager Server

This application provides a web service for task management. It is a goal of
this project to provide various backends, for example, DBMS, [Linked Data](
https://www.w3.org/standards/semanticweb/data) and [TaskWarrior](
https://taskwarrior.org/).

## Building

First install native Javascript dependencies with

    $ npm install

Install Purescript dependencies and build the project using
[Spago](https://github.com/purescript/spago):

    $ spago build

### Nix

If you have Nix installed, you can start a Nix shell with

    $ nix-shell

and then run

    $ npm install
    $ spago build

## Running

Once you've built the application, you can run it with

    $ spago run
