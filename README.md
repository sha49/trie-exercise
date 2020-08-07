# Trie-exercise

- 1) Transform a list of longitude, latitude coordinates into the corresponding list of GeoHash codes (precision 12).
- 2) Given the list of geohash words, you must provide the list of prefixes that uniquely identifies each word relative to the list.

# Requirements

- You only need `stack` installed on your machine

# Run and Test

```
> stack run
> stack test 
```

# Profiling

- If you want to analize time and memory usage you can use GHC tooling:

```
> stack clean
> stack build --profile
> stack exec --profile -- trie-exercise-exe +RTS -p    
> stack exec --profile -- trie-exercise-exe +RTS -h    
> stack exec -- hp2ps -e8in -c trie-exercise-exe.EXE.hp
```

# Implementation details

- Takes advantage of lazy haskell lists to process data in a stream.
- Profiling tools showed that geohash computation is the most expensive part of the code. This runs in parallel making a significant performance improvement.
- Words are stored in a Trie where each node constaint either Hashmap Char Trie or a Leaf String. That way shared prefixes are stored in the nodes and leaf contain the remaining lonely String of each word.
- Module Lib contains the Trie logic and Main the read/process/write logic.
- The output is written to the file resultat.txt
- In the test section there is some property testing for the resultant set of words.

# Performance

On my machine it takes around 1 second

Days              : 0
Hours             : 0
Minutes           : 0
Seconds           : 1
Milliseconds      : 10
Ticks             : 10103637
TotalDays         : 1.16940243055556E-05
TotalHours        : 0.000280656583333333
TotalMinutes      : 0.016839395
TotalSeconds      : 1.0103637
TotalMilliseconds : 1010.3637

# Possible improvement

- make usages of radix tree or patricia tree.
