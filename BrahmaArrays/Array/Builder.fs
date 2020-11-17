module BrahmaArrays.Builder

open Brahma.OpenCL
open WorkflowBasics.State.Builder
open FSharp.Quotations
open Brahma.FSharp.OpenCL.Core

type GpuContext(provider : ComputeProvider) =
    member this.Provider = provider
    member this.CommandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)


type GpuState<'result> = State<GpuContext, 'result>

type GpuWorkflow = StateBuilder<GpuContext>

let GpuRun (e : Expr<'range -> 'a>) (binder : ('range -> 'a) -> unit) : State<GpuContext, unit> =
    GpuWorkflow () {
        let! ctx = getState
        let _, kernelP, kernelR = ctx.Provider.Compile e

        binder kernelP        
        ctx.CommandQueue.Add(kernelR()).Finish() |> ignore
        do! putState ctx
        return ()
    }
