# Root level Makefile for project synchronization

include .env

.PHONY: sync

sync:
	rsync -avvz ./ ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}
