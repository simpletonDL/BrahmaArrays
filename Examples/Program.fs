open OpenCL.Net
open WorkflowBasics.State.Builder
open BrahmaArrays.Array
open Brahma.OpenCL
open BrahmaArrays.Builder


[<EntryPoint>]
let main _ =
//    let cpuPlatformName = "Intel(R) CPU*"
    let gpuPlatformName = "Intel(R) OpenCL HD*"

    let platformName = gpuPlatformName    
    let deviceType = DeviceType.Gpu

    let provider = ComputeProvider.Create(platformName, deviceType)
    let ctx = GpuContext provider

    let hostArray = [|1; 2; 3; 4|]
    
    let workflow =
        GpuWorkflow () {
            let! xs = ToGpu hostArray
            let! ys = GpuMap <@ fun x -> x * x + 10 @> xs
            let! zs = GpuMapSquare ys
            let! gs = GpuMap <@ fun x -> x + 1 @> zs
            return! ToHost gs
        }
    
    let res = execState workflow ctx
    printf "%A" res
    0
