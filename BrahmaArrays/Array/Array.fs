module BrahmaArrays.Array

open BrahmaArrays.Builder
open WorkflowBasics.State.Builder

open Brahma.OpenCL
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
    GpuWorkflow () {
        let! ctx = getState
        ctx.CommandQueue.Add(xs.ToGpu ctx.Provider).Finish () |> ignore
        return GpuArray xs
    }
    
let ToHost (GpuArray (xs : array<'a>)) : State<GpuContext, array<'a>> =
    GpuWorkflow () {
        let! ctx = getState
        ctx.CommandQueue.Add(xs.ToHost ctx.Provider).Finish () |> ignore
        return xs
    }

let MapSquare (GpuArray (input : array<int>)) =
    GpuWorkflow () {
        let xs = Array.zeroCreate input.Length
        let! output = ToGpu xs
        
        let code =
            <@
                fun (range : _1D) (input : array<int>) (output : array<int>) ->
                    let idx = range.GlobalID0
                    output.[idx] <- input.[idx] * input.[idx]
            @>

        let binder kernelP =
            let range = _1D <| input.Length
            kernelP range input xs
        
        do! GpuRun code binder    
        return output
    }