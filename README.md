# Atlas Examples

This repository contains examples that illustrate the various features of Atlas. Learn more about Atlas [here](https://atlas-app.io).

## Examples

- **bet-ref**: Find it's walkthrough [here](https://atlas-app.io/getting-started).
- **vesting**: Find it's walkthrough [here](https://www.youtube.com/watch?v=rapjgIuGWJw).

## Mint NFT
Output ref should have amount great than 5ADA
```
cd nft
cabal run mint-nft ./config.json ./minter.skey "a53d561a67ee84857ef282f4ec8c9b799d4c6fd95e7de33676fd1c5539e4ba94#0" 53505f677a77716a4f
```


## Format file
```
/root/.cabal/bin/stylish-haskell -r -i .
```

## test address 
addr_test1vzqqmthar9h3jhtf4fgdywcmxgq8c9wfc8pnmkua64qg4vgj9ugd0 

## oracle
init

```
cabal run oracle ./config.json ./minter.skey "6a3321f71abfcbda73341f8335b708835a1c47b7e4c6a9d55500f1f637fa7563#0" 160b0abfd68eb178b01fe2619f2127c89db7492d1958952dab371c21 53505f677a7771 2000000
```

update

```
cabal run oracle-update ./config.json ./minter.skey "b337c03a5a55f08cd710fb95261ed1d0a1f197628d819e4760a1ec2a2d6116f6#0" 160b0abfd68eb178b01fe2619f2127c89db7492d1958952dab371c21 53505f677a7771 3000000 2000000
```

delete
```
cabal run oracle-delete ./config.json ./minter.skey "8b84f7e60520d7795ee25235eb474d0277571115750ebbc5e2a28a5f86ba75bb#0" 160b0abfd68eb178b01fe2619f2127c89db7492d1958952dab371c21 53505f677a7771 3000000
```

## oracle server
- run server

```
cabal run oracle-server ./config.json ./minter.skey 
```

init

```
curl --location 'localhost:8081/oracle/init' \
--header 'Content-Type: application/json' \
--data '{
    "oref": "8230d77e236fd54dc0052c8a27b53a5ec4cb5f38490212b20c754109c31d4b01#0",
    "cs": "160b0abfd68eb178b01fe2619f2127c89db7492d1958952dab371c21",
    "tn": "53505f677a7771",
    "rate": 2000000
}'
```