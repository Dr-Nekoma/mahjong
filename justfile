# Lists all available targets
default:
    just --list

client:
	npm run --prefix ./client/ res:build

run:
	node client/src/main.res.mjs
