module BrahmaArrays.Builder

open Brahma.OpenCL
open WorkflowBasics.State.Builder

type GpuContext(provider : ComputeProvider) =
    member this.Provider = provider
    member this.CommandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)


type GpuState<'result> = State<GpuContext, 'result>

type GpuWorkflow = StateBuilder<GpuContext>
