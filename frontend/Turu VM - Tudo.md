
## Stack frame layout:

```
-- f's stack frame
<return address>
<frame pointer> <- frame point (FP)
<args>
<locals>
<working stack> <- sp
```

If we call another function:

* push return address
* push FP
* push args
* increment sp to make space by locals

So we get:

```
-- f's stack frame
<return address> -- &g
<frame pointer> <- frame pointer (FP)
<args>
<locals>
<working stack> <- sp
<return &f>
<framepointer>
<args>
<locals> <- sp
```

What about tail calls? f calling itself e.g.


`f x y = .... -> f x' y'`

We start with a stack frame like this:

```
-- f's stack frame
<return address>
<frame pointer> <- frame point (FP)
<args>
<locals>
<working stack> <- sp
```

Then during the tail call:

* replace arguments

## Compiling to bytecode

### Expressions

LitVal l
LitRef str

=> Data: [1: l]
   Code: Push l -- where does l come from?

   Answer: PushInt instruction

----------------

        let x = Nil
        in

=>

pushConDesc Nil ["Nil"]
AllocCon        [ClosureRef]

