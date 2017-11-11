[![Build Status](https://travis-ci.org/tsoding/nnaskell.svg?branch=master)](https://travis-ci.org/tsoding/nnaskell)

# Nnaskell

Neural Network in Haskell

## Quick Start

```console
$ nix-shell         # only for NixOS
$ stack build
$ stack ghci
> nn <- randomNN [2, 2, 1]
> xorTD
[([0.0,0.0],[0.0]),
 ([1.0,0.0],[1.0]),
 ([0.0,1.0],[1.0]),
 ([1.0,1.0],[0.0])]
> let trainedNN = head $ drop 1000 $ optimizeCost (cost xorTD) nn
> activateNN trainedNN $ vector [0.0, 0.0]
[4.045689665702377e-3]
> activateNN trainedNN $ vector [1.0, 0.0]
[0.9970477904605844]
>
```

## Support

You can support my work via

- Twitch channel: https://www.twitch.tv/subs/tsoding
- Patreon: https://www.patreon.com/tsoding
