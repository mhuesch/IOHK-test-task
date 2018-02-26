# IOHK test task

## To build

`stack build`

## To run

```
IOHKTT="\`stack path --dist-dir\`/build/iohktt/iohktt"
```
1. Launch (multiple) slaves with `$IOHKTT slave <ip> <port>`
2. Launch master with `$IOHKTT --send-for <send-for> --wait-for <wait-for> --with-seed <seed> master <ip> <port>`
3. Provided UDP multicast is enabled on the network where slaves & master are, master should discover the slaves and coordinate the run.

## Notes

- Help for iohktt can be accessed with `stack exec -- iohktt --help`
- Static linking can be enabled by uncommenting the noted line in the cabal file (see note there about caveat).

## Details

- This implementation uses [Vector Clocks](https://en.wikipedia.org/wiki/Vector_clock) to determine causal order. It accumulates a list of received messages, and sorts & prints them 1/4 of the way into the grace period (allowing 1/4 of the time for final messages to arrive, and 3/4 of the time for sorting & printing). For long send period durations, the resultant message queue can sometimes be very slow to sort, requiring a long grace period.
- Since vector clocks only afford a partial order on messages (some might be incomparable on account of happening concurrently in logical time), I use timestamps as a tiebreaker in the Ord instance.
- A single RNG seed is accepted in the master node, and is used to seed a MWC-random generator. Since MWC-random requires 256 Word32's of entropy, it will use an internal default seed in addition to our input seed. However, the generator is deterministic. The master generator is used to generate full entropy vectors for slaves. Thus, the initial seeds should be deterministic, and each slave will have a different entropy vector.
