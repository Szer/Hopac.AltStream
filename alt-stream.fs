namespace Hopac

open Hopac.Infixes
open Hopac.Extensions
open System.Collections.Generic

/// This is analog of `Hopac.Stream` but each `AltStream` is being represented as some `Alt<_>`
/// 
/// In contrast of `Hopac.Stream` this one **is not persistent**, every value going through AltStream only once,
/// so multiple readears will be concurrent to each other.
module AltStream =
    /// Underlying type of messages in AltStream. Could be:
    /// * `AltCons` - stream next value and link to next `Alt<AltCons<'x>>`
    /// * `AltNil` - stream end
    type AltCons<'x> =
        | AltCons of Value: 'x * AltStream<'x>
        | AltNil

    /// `AltStream` is just an `Alt` with `AltCons<'a>` inside it
    and AltStream<'x> = Alt<AltCons<'x>>

    /// Continuing a AltStream with `Alt.once`
    let inline cons<'x> (x:'x, next) : AltStream<'x> = Alt.once (AltCons (x, next))
    /// Continuing a AltStream with `Alt.once`. Args flipped
    let inline cons'<'x> next (x: 'x) : AltStream<'x> = Alt.once (AltCons (x, next))
    /// Closing AltStream with error on `read`
    let inline err<'x> e : AltStream<'x> = Alt.raises e
    /// Closing AltStream
    let nil<'x> : AltStream<'x> = Alt.always AltNil

    /// delaying stream creation with thunk
    let inline delayFun (u2xs: _ -> AltStream<_>) : AltStream<_> =
        Alt.prepareFun u2xs

    /// delaying stream creation with thunk
    let inline delayJob (u2xsJ: _ -> #Job<AltStream<_>>) : AltStream<_> =
        Alt.prepareJob u2xsJ

    /// creating stream from `IEnumerator`
    let rec ofEnum (xs: IEnumerator<'x>) : AltStream<'x> =
        delayFun <| fun _ ->
            try
                if xs.MoveNext() then
                    cons (xs.Current, ofEnum xs)
                else
                    xs.Dispose()
                    nil
            with e -> err e

    /// creating stream from `IEnumerable`
    let ofSeq (xs: #seq<'x>) : AltStream<'x> =
        delayFun (ofEnum << xs.GetEnumerator)

    /// creating infinite stream with same value `x`
    let rec indefinitely (x: 'x) : AltStream<'x> =
        cons (x, indefinitely x)

    /// Returns a stream that produces results whenever the given stream produces
    /// an element and the given job returns `Some` result from that element
    let rec chooseJob (x2yOJ: 'x -> #Job<'y option>) (xs: AltStream<'x>) : AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            Alt.prepare(
                x2yOJ x >>- function 
                | None -> chooseJob x2yOJ xs
                | Some x -> cons (x, chooseJob x2yOJ xs))
        | AltNil -> nil

    /// Returns a stream that produces results whenever the given stream produces
    /// an element and the given function returns `Some` result from that element
    let rec chooseFun (x2yO: 'x -> 'y option) (xs: AltStream<'x>) : AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            delayFun (fun _ ->
                match x2yO x with
                | None -> chooseFun x2yO xs
                | Some x -> cons (x, chooseFun x2yO xs))
        | AltNil -> nil

    /// `xs |> choose` is equivalent to `xs |> chooseFun id`.
    let choose (xs: AltStream<'x option>): AltStream<'x> = chooseFun id xs

    /// Returns a stream that contains the elements from the given stream for
    /// which the given job returns `true`
    let rec filterJob (x2bJ: 'x -> #Job<bool>) (xs: AltStream<'x>): AltStream<'x> =
        xs ^=> function
        | AltCons (x, xs) ->
            Alt.prepare(
                x2bJ x >>- fun b ->
                if b then cons (x, filterJob x2bJ xs)
                else filterJob x2bJ xs)
        | AltNil -> nil

    /// Returns a stream that contains the elements from the given stream for
    /// which the given function returns `true`
    let rec filterFun (x2b: 'x -> bool) (xs: AltStream<'x>): AltStream<'x> =
        xs ^=> function
        | AltCons (x, xs) ->
            delayFun(fun _ ->
                if x2b x then cons (x, filterFun x2b xs)
                else filterFun x2b xs)
        | AltNil -> nil

    /// Returns a stream that produces elements passed through the given alt
    /// whenever the given streams produces elements
    let rec mapAlt (x2yA: 'x -> #Alt<'y>) (xs: AltStream<'x>): AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            x2yA x ^=> cons' (mapAlt x2yA xs)
        | AltNil -> nil

    /// Returns a stream that produces elements passed through the given job
    /// whenever the given streams produces elements
    let rec mapJob (x2yJ: 'x -> #Job<'y>) (xs: AltStream<'x>): AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            x2yJ x
            >>- cons' (mapJob x2yJ xs)
            |> Alt.prepare
        | AltNil -> nil

    /// Returns a stream that produces elements passed through the given function
    /// whenever the given streams produces elements
    let rec mapFun (x2y: 'x -> 'y) (xs: AltStream<'x>): AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) -> cons (x2y x, mapFun x2y xs)
        | AltNil -> nil

    /// Returns a stream that produces the given element each time the given
    /// stream produces an element
    let rec mapConst (c:'c) (xs: AltStream<'x>): AltStream<'c> =
        xs ^=> function
        | AltCons (_, xs) -> cons (c, mapConst c xs)
        | AltNil -> nil

    /// `xs |> mapIgnore` is equivalent to `xs |> mapConst ()`
    let inline mapIgnore (xs: AltStream<'x>) : AltStream<unit> = 
        mapConst () xs

    /// Primitive version of `amb`.
    /// 
    /// Reference implementation:
    /// 
    /// ```fsharp
    /// ls <|> rs
    /// ```
    let inline amb' (ls: AltStream<'x>) (rs: AltStream<'x>) : AltStream<'x> = ls <|> rs

    /// Of the two given streams, returns the stream that first produces an
    /// element or is closed
    let rec amb (ls: AltStream<'x>) (rs: AltStream<'x>): AltStream<'x> =
        ls ^=> function
        | AltCons (l, ls) -> cons (l, amb ls rs)
        | AltNil -> nil
        <|>
        rs ^=> function
        | AltCons (r, rs) -> cons (r, amb ls rs)
        | AltNil -> nil

    /// Merge two streams together
    /// 
    /// Output stream will fail with last remain source error (if any)
    /// 
    /// Output stream will end when both source stream will end
    let rec merge (ls: AltStream<'x>) (rs: AltStream<'x>) : AltStream<'x> =
        ls ^=> function
        | AltCons (l, ls) -> cons (l, merge rs ls)
        | _ -> rs
        <|>
        rs ^=> function
        | AltCons (r, rs) -> cons (r, merge ls rs)
        | _ -> ls

    /// Returns stream which contains all `ls` values and after  all `rs` values
    /// 
    /// Output stream will fail when any of source produce error
    let rec append (ls: AltStream<'x>) (rs: AltStream<'x>): AltStream<'x> =
        ls ^=> function
        | AltCons (l, ls) -> cons (l, append ls rs)
        | AltNil -> rs

    /// Takes a stream and mapping job `'a -> #Job<#seq<'b>>`
    /// 
    /// Produces stream of `'b`. This is like stream version of `Seq.collect`
    let rec collectJob (x2ysJ: 'x -> #Job<#seq<'y>>) (xs: AltStream<'x>): AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            x2ysJ x
            >>- ofSeq
            >>- fun x -> append x (collectJob x2ysJ xs)
            |> Alt.prepare
        | AltNil -> nil

    /// Takes a stream and mapping function `'a -> 'b seq`
    /// 
    /// Produces stream of `'b`. This is like stream version of `Seq.collect`
    let rec collectFun (x2ys: 'x -> #seq<'y>) (xs: AltStream<'x>) : AltStream<'y> =
        xs ^=> function
        | AltCons (x, xs) ->
            delayFun (fun _ ->
                x2ys x
                |> ofSeq
                |> fun x -> append x (collectFun x2ys xs))
        | AltNil -> nil

    /// Eagerly reduces the given stream using the given job
    let rec foldJob (fJ: 's -> 'x -> #Job<'s>) (s: 's) (xs: AltStream<'x>): Job<'s> =
        xs >>= function
        | AltCons (x, xs) ->
            fJ s x 
            >>= fun s -> foldJob fJ s xs
        | AltNil -> Job.result s

    /// Eagerly reduces the given stream using the given function
    let rec foldFun (f: 's -> 'x -> 's) (s: 's) (xs: AltStream<'x>): Job<'s> =
        xs >>= function
        | AltCons (x, xs) ->
            f s x
            |> fun s -> foldFun f s xs
        | AltNil -> Job.result s

    /// Eagerly reduces the given stream of values to `list` of 
    /// values by prepending each new element
    let foldToList (xs: AltStream<'x>) : Job<'x list> = 
        foldFun (fun l x -> x::l) [] xs

    /// Eagerly reduces the given stream of sequence of values to `list` of values
    let foldXsToList (xs: AltStream<#seq<'x>>) : Job<'x list> =
        foldFun (Seq.fold (fun l x -> x::l)) [] xs

    /// Returns a job that computes the length of the given stream in `int64`
    let countL (xs: AltStream<'x>) : Job<int64> = foldFun (fun s _ -> s+1L) 0L xs
    /// Returns a job that computes the length of the given stream in `int32`
    let count  (xs: AltStream<'x>) : Job<int32> = foldFun (fun s _ -> s+1)  0  xs

    /// Returns a job that iterates the given job constructor over the given stream
    let rec iterJob (f: 'x -> #Job<unit>) (xs: AltStream<'x>) : Job<unit> =
        xs >>= function
        | AltCons (x, xs) ->
            f x
            >>=. iterJob f xs
        | AltNil -> Job.unit()

    // /// Returns a job that iterates the given function over the given stream
    let rec iterFun (f: 'x -> unit) (xs: AltStream<'x>) : Job<unit> = 
        xs >>= function
        | AltCons (x, xs) ->
            f x
            iterFun f xs
        | AltNil -> Job.unit()

    /// Returns a job that iterates over all the elements of the given stream.
    /// 
    /// `iter xs` is equivalent to `iterFun ignore xs`
    let rec iter (xs: AltStream<'x>) : Job<unit> = 
        xs >>= function
        | AltCons (_, xs) -> iter xs
        | AltNil -> Job.unit()

    /// `xs |> consumeJob x2uJ` is equivalent to `xs |> iterJob x2uJ |> queue`
    let consumeJob (fJ: 'x -> #Job<unit>) (xs: AltStream<'x>) : unit = iterJob fJ xs |> queue

    /// `xs |> consumeFun x2u` is equivalent to `xs |> iterFun x2u |> queue`
    let consumeFun (f: 'x -> unit) (xs: AltStream<'x>) : unit = iterFun f xs |> queue

    /// `xs |> consume` is equivalent to `xs |> iter |> queue`
    let consume (xs: AltStream<'x>): unit = iter xs |> queue

    let toResizeArray (xs: AltStream<'x>) : Job<ResizeArray<'x>> =
        Job.delay (fun _ ->
            let result = ResizeArray()
            iterFun result.Add xs
            >>-. result
        )

    /// `skip n xs` returns a stream without the first `n` elements of the given stream
    /// 
    /// If the given stream is shorter than `n`, then the returned stream will be empty
    /// 
    /// Note that if `n` is non-negative, then `append <| take n xs <| skip n xs` is equivalent to `xs`.
    let rec skip n (xs: AltStream<'x>): AltStream<'x> = delayFun (fun _ ->
        match n with
        | n when n < 0L -> failwith "skip: n < 0L"
        | 0L -> xs
        | n  ->
            xs ^=> function 
            | AltCons (_, xs) -> skip (n-1L) xs
            | AltNil -> nil)

    /// `take n` returns a stream that has the first `n` elements of the given stream
    /// 
    /// If the given stream is shorter than `n`, then `take n` is the identity function
    /// 
    /// Note that if `n` is non-negative, then `append <| take n xs <| skip n xs` is equivalent to `xs`
    let rec take n (xs: AltStream<'x>): AltStream<'x> = delayFun (fun _ ->
        match n with
        | n when n < 0L -> failwith "skip: n < 0L"
        | 0L -> nil
        | n  ->
            xs ^=> function 
            | AltCons (x, xs) -> cons(x, take (n-1L) xs)
            | AltNil -> nil)

    /// `mapParallelJob degree x2yJ xs` is like `mapJob x2yJ xs` except that up
    /// to `degree` number of jobs constructed with `x2yJ` may be run in parallel
    let mapParallelJob degree (x2yJ: 'x -> #Job<'y>) (xs: AltStream<'x>) : AltStream<'y> = 
        delayFun (fun _ ->

        if   degree < 1 then failwithf "degree must be 1 or greater, given %d" degree
        elif degree = 1 then mapJob x2yJ xs else

        let inCh, outCh = Ch(), Ch()
        let mutable usage = 0
        let closing = IVar()
        let rec loop() = 
            let workAlt =
                (outCh ^=> function
                 | Choice1Of2 y -> usage <- usage - 1
                                   cons (y, loop())
                 | Choice2Of2 e -> err e)
                <|>
                (if not (IVar.Now.isFull closing) && usage < degree then
                    inCh ^=> fun x ->
                        usage <- usage + 1
                        Job.tryInDelay 
                            (fun () -> x2yJ x)
                            (Choice1Of2 >> Ch.give outCh)
                            (Choice2Of2 >> Ch.give outCh)
                        |> Job.queue
                        >>= fun _ -> loop()
                 else Alt.never())
            let closingAlt = 
                (closing ^=> fun _ ->
                    if usage = 0 then nil
                    else workAlt)
            workAlt <|> closingAlt

        iterJob (Ch.give inCh) xs
        >>= IVar.fill closing
        |> queue
        loop())

    /// `mapParallelFun degree x2yJ xs` is like `mapJob x2yJ xs` except that up
    /// to `degree` number of jobs constructed with `x2yJ` may be run in parallel
    let mapParallelFun degree (x2y: 'x -> 'y) (xs: AltStream<'x>) : AltStream<'y> = 
        delayFun (fun _ ->

        if   degree < 1 then failwithf "degree must be 1 or greater, given %d" degree
        elif degree = 1 then mapFun x2y xs else

        let inCh, outCh = Ch(), Ch()
        let mutable usage = 0
        let closing = IVar()
        let rec loop() = 
            let workAlt =
                (outCh ^=> function
                 | Choice1Of2 y -> usage <- usage - 1
                                   cons (y, loop())
                 | Choice2Of2 e -> err e)
                <|>
                (if not (IVar.Now.isFull closing) && usage < degree then
                    inCh ^=> fun x ->
                        usage <- usage + 1
                        try
                            x2y x
                            |> Choice1Of2
                            |> Ch.give outCh
                        with e ->
                            Choice2Of2 e
                            |> Ch.give outCh
                        |> Job.queue
                        >>= loop
                 else Alt.never())
            let closingAlt = 
                (closing ^=> fun _ ->
                    if usage = 0 then nil
                    else workAlt)
            workAlt <|> closingAlt

        iterJob (Ch.give inCh) xs
        >>= IVar.fill closing
        |> queue
        loop())

    /// Returns a lazy stream that contains the elements 
    /// generated by the given job and initial state
    let rec unfoldJob (fJ: 's -> #Job<('x * 's) option>) (s: 's) : AltStream<'x> = 
        fJ s >>- function
        | None -> nil
        | Some (x,s) -> cons(x, unfoldJob fJ s)
        |> Alt.prepare

    /// Returns a lazy stream that contains the elements 
    /// generated by the given function and initial state
    let rec unfoldFun (f: 's -> ('x * 's) option) (s: 's) : AltStream<'x> =
        delayFun (fun _ ->
            match f s with
            | None -> nil
            | Some (x,s) -> cons(x, unfoldFun f s))

    type private Group<'k, 'x> = {key: 'k; var: Mailbox<MbCons<'x>>; }
    and private MbCons<'x> = 
        | Cons of 'x
        | Error of exn
        | Nil

    /// `groupByJob newGroup keyOf elems` splits the given source stream into
    /// substreams or groups based on the keys extracted from the elements by
    /// `keyOf` and formed using `newGroup`.  See also: `groupByFun`.
    /// 
    /// New groups are formed by calling the given function with a key, a job for
    /// closing the substream and the substream.  Unless explicitly closed,
    /// substreams remain alive as long as the source stream.  When closing
    /// substreams, it is important to understand that streams operate
    /// concurrently.  This means that one should always consume the substream
    /// until it ends after closing it.  If, after closing a substream, the given
    /// stream produces more elements with the same key, a new substream with the
    /// key will be opened.
    let groupByJob (newGroup: 'k -> Job<unit> -> AltStream<'x> -> #Job<'y>)
                   (keyOf: 'x -> #Job<'k>) (xs: AltStream<'x>) : AltStream<'y> =
        let key2br = Dictionary<'k, Group<'k,'x>>()

        let mainMb = Mailbox()

        let raised e =
            key2br.Values
            |> Seq.iterJob (fun g -> g.var *<<+ Error e)
            >>=. mainMb *<<+ Error e

        let allNil() =
            key2br.Values
            |> Seq.iterJob (fun g -> g.var *<<+ Nil)
            >>=. mainMb *<<+ Nil
      
        let rec mbToStream (mb: Mailbox<_>) : AltStream<_> =
            mb ^=> function
            | Cons x -> cons (x, mbToStream mb)
            | Error e -> err e
            | Nil -> nil

        let closes = Ch()

        let rec loop (xs: AltStream<'x>) : Alt<unit> =
            Alt.tryIn xs 
                <| function
                    | AltCons (x, xs) -> 
                        keyOf x >>= fun k ->
                            match key2br.TryGetValue k with
                            | true, g ->
                                g.var *<<+ Cons x
                                >>=. loop xs
                            | false, _ ->
                                let mb = Mailbox()
                                let g = { key=k; var=mb }
                                key2br.[k] <- g
                                let close = closes *<+ g
                                
                                newGroup k close (mbToStream mb)
                                >>=  (Cons >> Mailbox.send mainMb)
                                >>=. (g.var *<<+ Cons x)
                                >>=. loop xs
                    | AltNil -> allNil()
            <| raised
            <|> closes ^=> fun g ->
                match key2br.TryGetValue g.key with
                | true, g' when obj.ReferenceEquals (g', g) ->
                    key2br.Remove g.key |> ignore
                    g.var *<<+ Nil
                    >>=. loop xs
                | _ -> upcast loop xs
        loop xs |> queue
        mbToStream mainMb

    /// Trivial version of `groupByJob` without ability of closing substreams
    /// 
    /// Similar to stream version of `Seq.groupBy` where you only 
    /// need to pass key selector job
    let groupByIdJob (keyOfJ: 'x -> #Job<'k>) (xs: AltStream<'x>) : AltStream<'k * AltStream<'x>> =
        groupByJob
            (fun k _ xs -> Job.result (k, xs)) 
            keyOfJ xs

    /// `groupByFun newGroup keyOf elems` splits the given source stream into
    /// substreams or groups based on the keys extracted from the elements by
    /// `keyOf` and formed using `newGroup`
    /// 
    /// See also: `groupByJob` for further details
    let groupByFun (newGroup: 'k -> Job<unit> -> AltStream<'x> -> 'y)
                   (keyOf: 'x -> 'k) (xs: AltStream<'x>) : AltStream<'y> =
        let key2br = Dictionary<'k, Group<'k,'x>>()

        let mainMb = Mailbox()

        let raised e =
            key2br.Values
            |> Seq.iterJob (fun g -> g.var *<<+ Error e)
            >>=. mainMb *<<+ Error e

        let allNil() =
            key2br.Values
            |> Seq.iterJob (fun g -> g.var *<<+ Nil)
            >>=. mainMb *<<+ Nil
      
        let rec mbToStream (mb: Mailbox<_>) : AltStream<_> =
            mb ^=> function
            | Cons x -> cons (x, mbToStream mb)
            | Error e -> err e
            | Nil -> nil

        let closes = Ch()

        let rec loop (xs: AltStream<'x>) : Alt<unit> =
            Alt.tryIn xs 
                <| function
                    | AltCons (x, xs) ->
                        let k = keyOf x
                        match key2br.TryGetValue k with
                        | true, g ->
                            g.var *<<+ Cons x
                            >>=. loop xs
                        | false, _ ->
                            let mb = Mailbox()
                            let g = { key=k; var=mb }
                            key2br.[k] <- g
                            let close = closes *<+ g
                            let y = newGroup k close (mbToStream mb)
                            g.var *<<+ Cons x
                            >>=. mainMb *<<+ Cons y
                            >>=. loop xs
                    | AltNil -> allNil()
            <| raised
            <|> closes ^=> fun g ->
                match key2br.TryGetValue g.key with
                | true, g' when obj.ReferenceEquals (g', g) ->
                    key2br.Remove g.key |> ignore
                    g.var *<<+ Nil
                    >>=. loop xs
                | _ -> upcast loop xs
        loop xs |> queue
        mbToStream mainMb

    /// Trivial version of `groupByFun` without ability of closing substreams
    ///
    /// Similar to stream version of `Seq.groupBy` where you only 
    /// need to pass key selector function
    let groupByIdFun (keyOf: 'x -> 'k) (xs: AltStream<'x>) : AltStream<'k * AltStream<'x>> =
        groupByFun (fun k _ xs -> k, xs) keyOf xs
    
    /// Create `Stream<'x>` from `AltStream<'x>` 
    let toStream (xs: AltStream<'x>) : Stream<'x> =
        let src = Stream.Src.create<'x>()
        let stream = Stream.Src.tap src
        let rec loop xs =
            Job.tryIn xs
            <| function
                | AltCons (x, xs) -> Stream.Src.value src x >>=. loop xs
                | AltNil -> Stream.Src.close src
            <| Stream.Src.error src
        loop xs |> queue
        stream
      
    /// Create `AltStream<'x>` from `Stream<'x>`
    let rec ofStream (xs: Stream<'x>): AltStream<'x> =
        try xs >>- function
            | Hopac.Stream.Cons (x, xs) ->
                cons (x, ofStream xs)
            | Hopac.Stream.Nil ->
                nil
            |> Alt.prepare
        with e -> err e

    /// Memoize current stream to prevent any side effects on stream reiteration.
    let rec memoize (xs: AltStream<'x>) : AltStream<'x> = 
        (xs >>=* function
        | AltCons (x, xs) -> cons(x, memoize xs)
        | AltNil -> nil) :> Alt<_>

    /// Returns a `AltStream` whose elements are computed using the given function and initial state as with foldFun.
    let rec scanFun (f: 's -> 'x -> 's) (s: 's) (xs: AltStream<'x>): AltStream<'s> =
        xs ^=> function
        | AltCons (x, xs) ->
            f s x |> fun s ->
                cons (s, scanFun f s xs)
        | AltNil -> nil

    ///  Returns a `AltStream` whose elements are computed using the given job and initial state as with foldJob.
    let rec scanJob (fJ: 's -> 'x -> #Job<'s>) (s: 's) (xs: AltStream<'x>): AltStream<'s> =
        xs ^=> function
        | AltCons (x, xs) ->
            fJ s x >>- fun s ->
                cons (s, scanJob fJ s xs)
            |> Alt.prepare
        | AltNil -> nil
