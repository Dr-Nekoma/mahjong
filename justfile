# Lists all available targets
default:
    just --list

client:
	npm run --prefix ./client/ res:build

run:
	node client/src/main.res.mjs

serve:
	nix build && cd result/lib/node_modules/mahjong/ && npx serve dist
