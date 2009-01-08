## Template configuration

## Erlang Configuration
ERLC ?= erlc
EFLAGS ?= -Wall

## Revision Macro TAG
REV ?= $(shell svn info|grep Revision|awk '{print $$2};')

## Standard system Configuration
SED ?= sed
RM ?= rm
