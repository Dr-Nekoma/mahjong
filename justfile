# Lists all available targets
default:
    just --list

# Build client
client:
	npm run --prefix ./client/ res:build

# Serve client
serve: client
	nix build && cd result/lib/node_modules/mahjong/ && npx serve dist
