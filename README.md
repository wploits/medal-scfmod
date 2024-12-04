____
Looking for coder(s) to implement proper use of debug information for naming locals and parameters.
____

Make sure you have nightly version installed
```
rustup install nightly
```

Run luau-lifter
```
cargo +nightly run --bin luau-lifter <file>
```

Build luau-lifter.exe
```
cargo +nightly build --bin luau-lifter --release
```


Original README.md:

```
Medal's LuaU decompiler

All credits to this project goes to in honor and memory of:
Jujhar Singh (KowalskiFX)
Mathias Pedersen (Costomality)

While details of how they passed and our relationship with them are completely irrelevant its better if their legacy 
does not go in vain. 

Keep the Singh and Pedersen family in you guys prayers.
We love you both.
```