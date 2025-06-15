#!/bin/sh
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_INSTALL_HLS=1
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
wait
read -rsn1 -p "Press any key to exit"