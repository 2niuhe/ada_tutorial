# Root level Makefile for project synchronization

include .env

.PHONY: sync

sync:
	rsync -avz --exclude .git/ ./ $${REMOTE_USER}@$${REMOTE_HOST}:$${REMOTE_PATH}
