module BrahmaArrays.Array

open BrahmaArrays.Builder
open WorkflowBasics.State.Builder

open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions

type GpuArray<'a> = GpuArray of array<'a>

let ToGpu (xs : array<'a>) =
    State <|
        fun (ctx : GpuContext) ->
            try
                ctx.CommandQueue.Add(xs.ToGpu ctx.Provider).Finish () |> ignore
            with _ -> ()
            GpuArray xs, ctx
            
let ToGpu' (xs : array<'a>) : State<GpuContext, GpuArray<'a>> =
    StateBuilder () {
        let! ctx = getState
        ctx.CommandQueue.Add(xs.ToGpu ctx.Provider).Finish () |> ignore
        return GpuArray xs
    }
    
let ToHost (GpuArray (xs : array<'a>)) : State<GpuContext, array<'a>> =
    StateBuilder () {
        let! ctx = getState
        ctx.CommandQueue.Add(xs.ToHost ctx.Provider).Finish () |> ignore
        return xs
    }

let MapSquare (GpuArray (input : array<int>)) =
    State <|
        fun (ctx : GpuContext) ->
            let xs = Array.zeroCreate input.Length
            let GpuArray output, ctx = runState <| ToGpu xs <| ctx
            
            let code =
                <@
                    fun (range : _1D) (input : array<int>) (output : array<int>) ->
                        let idx = range.GlobalID0
                        output.[idx] <- input.[idx] * input.[idx]
                @>
            let _, kernelP, kernelR = ctx.Provider.Compile code
            let range = _1D <| input.Length
            kernelP range input output
            ctx.CommandQueue
                .Add(kernelR())
                .Finish() |> ignore
            GpuArray output, ctx

let MapSquare' (GpuArray (input : array<int>)) =
    StateBuilder () {
        let! ctx = getState
        
        let xs = Array.zeroCreate input.Length
        let! output = ToGpu xs
        
        let code =
            <@
                fun (range : _1D) (input : array<int>) (output : array<int>) ->
                    let idx = range.GlobalID0
                    output.[idx] <- input.[idx] * input.[idx]
            @>

        let _, kernelP, kernelR = ctx.Provider.Compile code
        let range = _1D <| input.Length
        kernelP range input xs
        ctx.CommandQueue
            .Add(kernelR())
            .Finish() |> ignore
        return output
    }