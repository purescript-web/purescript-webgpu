module Sandbox where

import Prelude

import Control.Lazy (fix)
import Control.Monad.ST.Class (liftST)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.Array.ST as STArray
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Typed (class TypedArray, fromArray, setTyped, toArray, whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (ArrayView, Uint16Array, Float32Array)
import Data.Float32 (Float32)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Int.Bits (complement, (.&.))
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pi)
import Data.Number as Math
import Data.Number.Format (precision, toStringWith)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)

import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (setAttribute, toNode)
import Web.DOM.Node (setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.BufferSource (fromFloat32Array)
import Web.GPU.GPU (requestAdapter)
import Web.GPU.GPUAdapter (requestDevice)
import Web.GPU.GPUBindGroupEntry (GPUBufferBinding, gpuBindGroupEntry)
import Web.GPU.GPUBindGroupLayoutEntry (gpuBindGroupLayoutEntry)
import Web.GPU.GPUBuffer (GPUBuffer, getMappedRange, mapAsync, unmap)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUBufferBindingType as GPUBufferBindingType
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUColor (gpuColorDict)
import Web.GPU.GPUColorTargetState (GPUColorTargetState)
import Web.GPU.GPUCommandEncoder (beginComputePass, beginRenderPass, copyBufferToBuffer, finish)
import Web.GPU.GPUCompareFunction as GPUCompareFunction
import Web.GPU.GPUComputePassEncoder as GPUComputePassEncoder
import Web.GPU.GPUCullMode as GPUCullMode
import Web.GPU.GPUDepthStencilState (GPUDepthStencilState)
import Web.GPU.GPUDevice (createBindGroup, createBindGroupLayout, createBuffer, createCommandEncoder, createComputePipeline, createPipelineLayout, createRenderPipeline, createShaderModule, createTexture)
import Web.GPU.GPUDevice as GPUDevice
import Web.GPU.GPUExtent3D (gpuExtent3DWHD)
import Web.GPU.GPUFragmentState (GPUFragmentState)
import Web.GPU.GPUFrontFace (cw)
import Web.GPU.GPUIndexFormat (uint16)
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUMapMode as GPUMapMode
import Web.GPU.GPUPrimitiveState (GPUPrimitiveState)
import Web.GPU.GPUPrimitiveTopology (triangleList)
import Web.GPU.GPUProgrammableStage (GPUProgrammableStage)
import Web.GPU.GPUQueue (submit, writeBuffer)
import Web.GPU.GPURenderPassColorAttachment (GPURenderPassColorAttachment)
import Web.GPU.GPURenderPassDepthStencilAttachment (GPURenderPassDepthStencilAttachment)
import Web.GPU.GPURenderPassDescriptor (GPURenderPassDescriptor)
import Web.GPU.GPURenderPassEncoder (drawIndexedWithInstanceCount, end, setBindGroup, setIndexBuffer, setPipeline, setScissorRect, setVertexBuffer, setViewport)
import Web.GPU.GPURenderPipelineDescriptor (GPURenderPipelineDescriptor)
import Web.GPU.GPUShaderStage as GPUShaderStage
import Web.GPU.GPUStoreOp as GPUStoreOp
import Web.GPU.GPUTexture (createView)
import Web.GPU.GPUTextureDescriptor (GPUTextureDescriptor)
import Web.GPU.GPUTextureFormat as GPUTextureFormat
import Web.GPU.GPUTextureUsage as GPUTextureUsage
import Web.GPU.GPUVertexAttribute (GPUVertexAttribute)
import Web.GPU.GPUVertexBufferLayout (GPUVertexBufferLayout)
import Web.GPU.GPUVertexFormat (float32x3)
import Web.GPU.GPUVertexState (GPUVertexState)
import Web.GPU.GPUVertexStepMode as StepMode
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.Bitwise ((.|.))
import Web.GPU.Internal.RequiredAndOptional (x)
import Web.GPU.Navigator (gpu)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, height, width)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, navigator, requestAnimationFrame)
import Web.Promise as Web.Promise

averager :: forall a. EuclideanRing a => Effect (a -> Effect a)
averager = do
  ct <- Ref.new zero
  val <- Ref.new zero
  pure \v -> do
    ct' <- Ref.read ct
    val' <- Ref.read val
    Ref.write (ct' + one) ct
    Ref.write (val' + v) val
    pure $ val' / ct'

hackyFloatConv :: Array Number -> Array Float32
hackyFloatConv = unsafeCoerce

hackyIntConv :: Array Int -> Array UInt
hackyIntConv = unsafeCoerce

convertPromise :: Web.Promise.Promise ~> Control.Promise.Promise
convertPromise = unsafeCoerce

-- a port of alaingalvan/webgpu-seed

showErrorMessage :: Effect Unit
showErrorMessage = do
  d <- window >>= document
  getElementById "error" (toNonElementParentNode d) >>= traverse_
    (setAttribute "style" "display:auto; color: white;")

freshIdentityMatrix :: Effect Float32Array
freshIdentityMatrix = fromArray $ hackyFloatConv
  [ 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

freshTranslateMatrix
  :: Number
  -> Number
  -> Number
  -> Effect Float32Array
freshTranslateMatrix x y z = fromArray $ hackyFloatConv
  -- column major! transpose at the bottom
  [ 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , x
  , y
  , z
  , 1.0
  ]

getPerspectiveMatrix ∷ Effect Float32Array
getPerspectiveMatrix = do
  let
    fovy = pi / 2.0
    aspect = 1.0
    f = 1.0 / Math.tan (fovy / 2.0)
    near = 0.1
    far = 10.0
    nf = 1.0 / (near - far)
  -- column major! transpose at the bottom
  perspectiveData :: Float32Array <- fromArray $ hackyFloatConv
    [ f / aspect
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , f
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , far * nf
    , (-1.0)
    , 0.0
    , 0.0
    , far * near * nf
    , 0.0
    ]
  pure perspectiveData

main :: Effect Unit
main = do
  timeDeltaAverager <- averager
  frameDeltaAverager <- averager
  startsAt <- getTime <$> now
  doc <- window >>= document
  renderStats <-
    ( getElementById "render-stats"
        (toNonElementParentNode doc)
    ) >>= maybe
      (showErrorMessage *> throwError (error "could not find render-stats div"))
      pure
  positions :: Float32Array <- fromArray $ hackyFloatConv
    [ 1.0
    , 1.0
    , 1.0
    --
    , 1.0
    , 1.0
    , -1.0
    --
    , 1.0
    , -1.0
    , 1.0
    --
    , -1.0
    , 1.0
    , 1.0
    --
    , -1.0
    , -1.0
    , 1.0
    --
    , -1.0
    , 1.0
    , -1.0
    --
    , 1.0
    , -1.0
    , -1.0
    --
    , -1.0
    , -1.0
    , -1.0
    ]

  -- 🎨 Color Vertex Buffer Data
  colors :: Float32Array <- fromArray $ hackyFloatConv
    [ 0.94
    , 0.97
    , 1.0
    , --
      0.82
    , 0.41
    , 0.11
    , --
      1.0
    , 0.75
    , 0.79
    , --
      0.25
    , 0.41
    , 0.88
    , --
      0.98
    , 0.5
    , 0.44
    , --
      0.96
    , 0.87
    , 0.7
    , --
      0.96
    , 0.96
    , 0.96
    , --
      1.0
    , 0.89
    , 0.7
    ]
  -- [0.4,0.5,0.4,1.0,1.0,1.0,0.4,0.5,0.4,1.0,1.0,1.0,0.4,0.5,0.4,1.0,1.0,1.0,0.4,0.5,0.4,1.0,1.0,1.0]
  uniformData :: Float32Array <- fromArray $ hackyFloatConv
    [ 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    ,
      -- 🔴 Primary Color
      0.9
    , 0.1
    , 0.3
    , 1.0
    ,
      -- 🟣 Accent Color
      0.8
    , 0.2
    , 0.8
    , 1.0
    ]
  imx <- freshIdentityMatrix
  currentFrame <- Ref.new 0
  let
    scaleData = imx
  -- timestamp, currentFrame
  timeData :: Float32Array <- fromArray $ hackyFloatConv [ 0.0, 0.0 ]
  let
    rotateZData = imx
    rotateZResultData = imx
    rotateXData = imx
    rotateXResultData = imx
    rotateYData = imx
    rotateYResultData = imx
  translateZData :: Float32Array <- map identity $ freshTranslateMatrix 0.0 0.0
    (-1.5)
  let
    translateZResultData = imx
  perspectiveData :: Float32Array <- getPerspectiveMatrix
  let
    perspectiveResultData = imx
  -- 📇 Index Buffer Data
  outputBuffers <- liftST $ STArray.new
  indices :: Uint16Array <- fromArray $ hackyIntConv
    [
      --
      0
    , 2
    , 1
    ----
    , 1
    , 2
    , 6
    --
    , 6
    , 2
    , 7
    ----
    , 7
    , 2
    , 4
    --
    , 5
    , 7
    , 4
    ----
    , 5
    , 4
    , 3
    --
    , 5
    , 3
    , 1
    ----
    , 1
    , 3
    , 0
    --
    , 5
    , 1
    , 7
    ----
    , 1
    , 6
    , 7
    --
    , 3
    , 4
    , 0
    ----
    , 0
    , 4
    , 2
    ]
  -- 🏭 Entry to WebGPU
  entry <- window >>= navigator >>= gpu >>= case _ of
    Nothing -> do
      showErrorMessage
      throwError $ error "WebGPU is not supported"
    Just entry -> pure entry
  launchAff_ do
    adapter <- (toAffE $ convertPromise <$> requestAdapter entry (x {})) >>=
      case _ of
        Nothing -> liftEffect do
          showErrorMessage
          throwError $ error "WebGPU is not supported"
        Just adapter -> pure adapter
    device <- (toAffE $ convertPromise <$> requestDevice adapter (x {})) >>=
      case _ of
        Nothing -> liftEffect do
          showErrorMessage
          throwError $ error "WebGPU is not supported"
        Just device -> pure device
    -- 📦 Queue
    queue <- liftEffect $ GPUDevice.queue device
    let
      createBufferF
        :: forall a t
         . TypedArray a t
        => ArrayView a
        -> GPUBufferUsageFlags
        -> Effect GPUBuffer
      createBufferF arr usage = do
        let
          desc = x
            { size: ((byteLength (Typed.buffer arr)) + 3) .&. complement 3
            , usage
            , mappedAtCreation: true
            }
        buffer <- createBuffer device desc
        writeArray <- getMappedRange buffer >>= whole
        _ <- setTyped writeArray Nothing arr
        unmap buffer
        pure buffer
    positionBuffer <- liftEffect $ createBufferF positions GPUBufferUsage.vertex
    colorBuffer <- liftEffect $ createBufferF colors GPUBufferUsage.vertex
    indexBuffer <- liftEffect $ createBufferF indices GPUBufferUsage.index
    -- ✋ Declare buffer handles
    let standardStorageFlag = GPUBufferUsage.storage
    let finalStorageFlag = GPUBufferUsage.storage .|. GPUBufferUsage.copySrc
    uniformBuffer <- liftEffect $ createBufferF uniformData
      (GPUBufferUsage.uniform .|. GPUBufferUsage.copyDst)
    timeBuffer <- liftEffect $ createBufferF timeData
      ( GPUBufferUsage.storage .|. GPUBufferUsage.copyDst .|.
          GPUBufferUsage.copySrc
      )
    scaleBuffer <- liftEffect $ createBufferF scaleData
      standardStorageFlag
    rotateZBuffer <- liftEffect $ createBufferF rotateZData
      standardStorageFlag
    rotateZResultBuffer <- liftEffect $ createBufferF rotateZResultData
      standardStorageFlag
    rotateXBuffer <- liftEffect $ createBufferF rotateXData
      standardStorageFlag
    rotateXResultBuffer <- liftEffect $ createBufferF rotateXResultData
      standardStorageFlag
    rotateYBuffer <- liftEffect $ createBufferF rotateYData
      standardStorageFlag
    rotateYResultBuffer <- liftEffect $ createBufferF rotateYResultData
      standardStorageFlag
    translateZBuffer <- liftEffect $ createBufferF translateZData
      standardStorageFlag
    translateZResultBuffer <- liftEffect $ createBufferF translateZResultData
      standardStorageFlag
    perspectiveBuffer <- liftEffect $ createBufferF perspectiveData
      standardStorageFlag
    perspectiveResultBuffer <- liftEffect $ createBufferF perspectiveResultData
      finalStorageFlag
    -- 🖍️ Shaders
    let
      initialScaleDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : array<f32>;

@compute @workgroup_size(4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  // We use a power of 2 above but we only need 3
  if (global_id.x >= 3u) {
    return;
  }

  resultMatrix[global_id.x*4 + global_id.x] = 0.25;
}"""
        }
    initialScaleModule <- liftEffect $ createShaderModule device
      initialScaleDesc
    let
      rotateZDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : array<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159;
  var o = (x << 1) + y;
  return sin((time * pi) + (f32(2 - ((o + 1) % 3)) * (pi / 2.0)));
}

@compute @workgroup_size(4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ixx = global_id.x / 2;
  var ixy = global_id.x % 2;
  resultMatrix[ixx*4 + ixy] = xyt2trig(ixx, ixy, time);
}"""
        }
    rotateZModule <- liftEffect $ createShaderModule device rotateZDesc
    let
      rotateYDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : array<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159;
  var o = (x << 1) + y;
  return sin((time * pi) + (f32(2 - ((o + 1) % 3)) * (pi / 2.0)));
}

@compute @workgroup_size(4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var ixx = global_id.x / 2;
  var ixy = global_id.x % 2;

  resultMatrix[(ixx*8) + (ixy*2)] = xyt2trig(ixx, ixy, time);
}"""
        }
    rotateYModule <- liftEffect $ createShaderModule device rotateYDesc
    let
      rotateXDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : array<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159; 
  var o = (x << 1) + y;
  return sin((time * pi) + (f32((o + 1) % 3) * (pi / 2.0)));
}

@compute @workgroup_size(4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  var ixx = global_id.x / 2;
  var ixy = global_id.x % 2;

  resultMatrix[1 + ((ixx + 1)*4) + ixy] = xyt2trig(ixx, ixy, time);
}"""
        }
    rotateXModule <- liftEffect $ createShaderModule device rotateXDesc
    let
      matrixMultiplicationDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read> matrixL : array<f32>;
@group(0) @binding(1) var<storage, read> matrixR : array<f32>;
@group(0) @binding(2) var<storage, read_write> resultMatrix : array<f32>;
@compute @workgroup_size(16)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  var result = 0.0;
  var ixx = global_id.x / 4;
  var ixy = global_id.x % 4;
  for (var i = 0u; i < 4u; i = i + 1u) {
    result = result + (matrixL[(i * 4 )+ ixy] * matrixR[(ixx * 4) + i]);
  }

  resultMatrix[ixx*4 + ixy] = result;
}"""
        }
    matrixMultiplicationModule <- liftEffect $ createShaderModule device
      matrixMultiplicationDesc
    let
      vsmDesc =
        x
          { code:
              """
struct UBO {
  modelViewProj: mat4x4<f32>,
  primaryColor: vec4<f32>,
  accentColor: vec4<f32>
};

struct VSOut {
    @builtin(position) Position: vec4<f32>,
    @location(0) color: vec3<f32>,
};

@group(0) @binding(0)
var<uniform> uniforms: UBO;

@vertex
fn main(@location(0) inPos: vec3<f32>,
        @location(1) inColor: vec3<f32>) -> VSOut {
    var vsOut: VSOut;
    vsOut.Position = uniforms.modelViewProj * vec4<f32>(inPos, 1.0);
    vsOut.color = inColor;
    return vsOut;
}
"""
          }
    vertModule <- liftEffect $ createShaderModule device vsmDesc
    let
      fsmDesc =
        x
          { code:
              """
@fragment
fn main(@location(0) inColor: vec3<f32>) -> @location(0) vec4<f32> {
    return vec4<f32>(inColor, 1.0);
}
"""
          }
    fragModule <- liftEffect $ createShaderModule device fsmDesc

    -- ⚗️ Graphics Pipeline

    -- 🔣 Input Assembly
    let
      (positionAttribDesc :: GPUVertexAttribute) = x
        { shaderLocation: 0
        , -- [[location(0)]]
          offset: 0
        , format: float32x3
        }
    let
      (colorAttribDesc :: GPUVertexAttribute) = x
        { shaderLocation: 1
        , -- [[location(1)]]
          offset: 0
        , format: float32x3
        }
    let
      (positionBufferDesc :: GPUVertexBufferLayout) = x
        { attributes: [ positionAttribDesc ]
        , arrayStride: 4 * 3
        -- sizeof(float) * 3
        , stepMode: StepMode.vertex
        }
    let
      (colorBufferDesc :: GPUVertexBufferLayout) = x
        { attributes: [ colorAttribDesc ]
        , arrayStride: 4 * 3
        -- sizeof(float) * 3
        , stepMode: StepMode.vertex
        }

    -- 🌑 Depth
    let
      (depthStencil :: GPUDepthStencilState) = x
        { format: GPUTextureFormat.depth24plus -- Stencil8
        , depthWriteEnabled: true
        , depthCompare: GPUCompareFunction.less
        }
    -- 🗺️ bind group layouts
    uniformBindGroupLayout <- liftEffect $ createBindGroupLayout device $ x
      { entries:
          [ gpuBindGroupLayoutEntry 0 GPUShaderStage.vertex
              ( x { type: GPUBufferBindingType.uniform }
                  :: GPUBufferBindingLayout
              )
          ]
      }
    simpleIOComputeBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    matrixMultiplicationComputeBindGroupLayout <- liftEffect
      $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 1 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              , gpuBindGroupLayoutEntry 2 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.storage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    timeBindGroupLayout <- liftEffect $ createBindGroupLayout device
      $ x
          { entries:
              [ gpuBindGroupLayoutEntry 0 GPUShaderStage.compute
                  ( x { type: GPUBufferBindingType.readOnlyStorage }
                      :: GPUBufferBindingLayout
                  )
              ]
          }
    -- 🗄️ Bind Group
    -- ✍ This would be used when *encoding commands*
    uniformBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: uniformBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: uniformBuffer } :: GPUBufferBinding)
          ]
      }
    scaleBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: simpleIOComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: scaleBuffer } :: GPUBufferBinding)
          ]
      }
    rotateZComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: simpleIOComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateZBuffer } :: GPUBufferBinding)
          ]
      }
    rotateZResultIOComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: matrixMultiplicationComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateZBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: scaleBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: rotateZResultBuffer } :: GPUBufferBinding)
          ]
      }
    rotateXComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: simpleIOComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateXBuffer } :: GPUBufferBinding)
          ]
      }
    rotateXResultIOComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: matrixMultiplicationComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateXBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: rotateZResultBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: rotateXResultBuffer } :: GPUBufferBinding)
          ]
      }
    rotateYComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: simpleIOComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateYBuffer } :: GPUBufferBinding)
          ]
      }
    rotateYResultIOComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: matrixMultiplicationComputeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: rotateYBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 1
              (x { buffer: rotateXResultBuffer } :: GPUBufferBinding)
          , gpuBindGroupEntry 2
              (x { buffer: rotateYResultBuffer } :: GPUBufferBinding)
          ]
      }
    translateZResultIOComputeBindGroup <- liftEffect $ createBindGroup device $
      x
        { layout: matrixMultiplicationComputeBindGroupLayout
        , entries:
            [ gpuBindGroupEntry 0
                (x { buffer: translateZBuffer } :: GPUBufferBinding)
            , gpuBindGroupEntry 1
                (x { buffer: rotateYResultBuffer } :: GPUBufferBinding)
            , gpuBindGroupEntry 2
                (x { buffer: translateZResultBuffer } :: GPUBufferBinding)
            ]
        }
    perspectiveResultIOComputeBindGroup <- liftEffect $ createBindGroup device $
      x
        { layout: matrixMultiplicationComputeBindGroupLayout
        , entries:
            [ gpuBindGroupEntry 0
                (x { buffer: perspectiveBuffer } :: GPUBufferBinding)
            , gpuBindGroupEntry 1
                (x { buffer: translateZResultBuffer } :: GPUBufferBinding)
            , gpuBindGroupEntry 2
                (x { buffer: perspectiveResultBuffer } :: GPUBufferBinding)
            ]
        }
    timeComputeBindGroup <- liftEffect $ createBindGroup device $ x
      { layout: timeBindGroupLayout
      , entries:
          [ gpuBindGroupEntry 0
              (x { buffer: timeBuffer } :: GPUBufferBinding)
          ]
      }
    -- 🦄 Bind group layouts
    renderLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ uniformBindGroupLayout ] }
    simpleIOComputeLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts: [ simpleIOComputeBindGroupLayout ] }
    ioPlusTComputeLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ simpleIOComputeBindGroupLayout, timeBindGroupLayout ]
      }
    matrixMultiplicationLayout <- liftEffect $ createPipelineLayout device $ x
      { bindGroupLayouts:
          [ matrixMultiplicationComputeBindGroupLayout ]
      }
    -- 🎭 Shader Stages
    let
      (initialScaleState :: GPUProgrammableStage) = x
        { "module": initialScaleModule
        , entryPoint: "main"
        }
      (rotateZState :: GPUProgrammableStage) = x
        { "module": rotateZModule
        , entryPoint: "main"
        }
      (rotateYState :: GPUProgrammableStage) = x
        { "module": rotateYModule
        , entryPoint: "main"
        }
      (rotateXState :: GPUProgrammableStage) = x
        { "module": rotateXModule
        , entryPoint: "main"
        }
      (matrixMultiplicationState :: GPUProgrammableStage) = x
        { "module": matrixMultiplicationModule
        , entryPoint: "main"
        }
      (vertex :: GPUVertexState) = x
        { "module": vertModule
        , entryPoint: "main"
        , buffers: [ positionBufferDesc, colorBufferDesc ]
        }

    -- 🌀 Color/Blend State
    let
      (colorState :: GPUColorTargetState) = x
        { format: GPUTextureFormat.bgra8unorm
        }

    let
      (fragment :: GPUFragmentState) = x
        { "module": fragModule
        , entryPoint: "main"
        , targets: [ colorState ]
        }

    -- 🟨 Rasterization
    let
      (primitive :: GPUPrimitiveState) = x
        { frontFace: cw
        , cullMode: GPUCullMode.none
        , topology: triangleList
        }

    let
      (pipelineDesc :: GPURenderPipelineDescriptor) = x
        { layout: renderLayout
        , vertex
        , fragment
        , primitive
        , depthStencil
        }
    initialScalePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: simpleIOComputeLayout
      , compute: initialScaleState
      }
    rotateZPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: ioPlusTComputeLayout
      , compute: rotateZState
      }
    rotateYPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: ioPlusTComputeLayout
      , compute: rotateYState
      }
    rotateXPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: ioPlusTComputeLayout
      , compute: rotateXState
      }
    matrixMultiplicationPipeline <- liftEffect $ createComputePipeline device $
      x
        { layout: matrixMultiplicationLayout
        , compute: matrixMultiplicationState
        }
    renderPipeline <- liftEffect $ createRenderPipeline device pipelineDesc
    { canvasWidth, canvasHeight, context } <- liftEffect do

      canvas <-
        ( (_ >>= fromElement) <$> getElementById "gfx"
            (toNonElementParentNode doc)
        ) >>= maybe
          (showErrorMessage *> throwError (error "could not find canvas"))
          pure

      context <- getContext canvas >>= maybe
        (showErrorMessage *> throwError (error "could not find context"))
        pure
      canvasWidth <- width canvas
      canvasHeight <- height canvas
      pure { context, canvasWidth, canvasHeight }
    let
      (config :: GPUCanvasConfiguration) = x
        { device
        , format: GPUTextureFormat.bgra8unorm
        , usage: GPUTextureUsage.renderAttachment
        , alphaMode: opaque
        }
    liftEffect $ configure context config
    let
      (depthTextureDesc :: GPUTextureDescriptor) = x
        { size: gpuExtent3DWHD canvasWidth canvasHeight 1
        , format: GPUTextureFormat.depth24plus
        , usage: GPUTextureUsage.renderAttachment -- .|. GPUTextureUsage.copySrc
        --, dimension: GPUTextureDimension.twoD
        }
    depthTexture <- liftEffect $ createTexture device depthTextureDesc
    depthTextureView <- liftEffect $ createView depthTexture
    let
      encodeCommands colorTextureView = do
        let
          (colorAttachment :: GPURenderPassColorAttachment) = x
            { view: colorTextureView
            , loadOp: GPULoadOp.clear
            , storeOp: GPUStoreOp.store
            , clearValue: gpuColorDict { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
            }

        let
          (depthAttachment :: GPURenderPassDepthStencilAttachment) = x
            { view: depthTextureView
            , depthClearValue: 1.0
            , depthLoadOp: GPULoadOp.clear
            , depthStoreOp: GPUStoreOp.store
            }

        commandEncoder <- createCommandEncoder device (x {})

        -- 💻 Encode compute commands
        scalePassEncoder <- beginComputePass commandEncoder (x {})
        tn <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
        cf <- Ref.read currentFrame
        timeNowData :: Float32Array <- fromArray $ hackyFloatConv
          [ (tn / 2.0), toNumber cf ]
        Ref.modify_ (add 1) currentFrame
        writeBuffer queue timeBuffer 0 (fromFloat32Array timeNowData)
        GPUComputePassEncoder.setPipeline scalePassEncoder initialScalePipeline
        GPUComputePassEncoder.setBindGroup scalePassEncoder 0
          scaleBindGroup
        GPUComputePassEncoder.dispatchWorkgroups scalePassEncoder 1
        GPUComputePassEncoder.end scalePassEncoder
        ---------------------
        rotateZPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateZPassEncoder
          rotateZPipeline
        GPUComputePassEncoder.setBindGroup rotateZPassEncoder 0
          rotateZComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateZPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateZPassEncoder 1
        GPUComputePassEncoder.end rotateZPassEncoder
        ---------------------
        rotateZMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateZMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateZMulPassEncoder 0
          rotateZResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateZMulPassEncoder 1
        GPUComputePassEncoder.end rotateZMulPassEncoder
        ---------------------
        rotateXPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateXPassEncoder
          rotateXPipeline
        GPUComputePassEncoder.setBindGroup rotateXPassEncoder 0
          rotateXComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateXPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateXPassEncoder 1
        GPUComputePassEncoder.end rotateXPassEncoder
        ---------------------
        rotateXMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateXMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateXMulPassEncoder 0
          rotateXResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateXMulPassEncoder 1
        GPUComputePassEncoder.end rotateXMulPassEncoder
        ---------------------
        rotateYPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateYPassEncoder
          rotateYPipeline
        GPUComputePassEncoder.setBindGroup rotateYPassEncoder 0
          rotateYComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateYPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateYPassEncoder 1
        GPUComputePassEncoder.end rotateYPassEncoder
        ---------------------
        rotateYMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateYMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateYMulPassEncoder 0
          rotateYResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups rotateYMulPassEncoder 1
        GPUComputePassEncoder.end rotateYMulPassEncoder
        ---------------------
        translateZMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline translateZMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup translateZMulPassEncoder 0
          translateZResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups translateZMulPassEncoder 1
        GPUComputePassEncoder.end translateZMulPassEncoder
        ---------------------
        perspectiveMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline perspectiveMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup perspectiveMulPassEncoder 0
          perspectiveResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroups perspectiveMulPassEncoder 1
        GPUComputePassEncoder.end perspectiveMulPassEncoder

        copyBufferToBuffer commandEncoder perspectiveResultBuffer 0
          uniformBuffer
          0
          (4 * 16)
        -- 🖌️ Encode drawing commands
        let
          (renderPassDesc :: GPURenderPassDescriptor) = x
            { colorAttachments: [ colorAttachment ]
            , depthStencilAttachment: depthAttachment
            }
        passEncoder <- beginRenderPass commandEncoder renderPassDesc
        -- Set the viewport
        setViewport passEncoder
          0.0
          0.0
          (toNumber canvasWidth)
          (toNumber canvasHeight)
          0.0
          1.0
        setScissorRect passEncoder
          0
          0
          canvasWidth
          canvasHeight
        -- Set the pipeline
        setPipeline passEncoder renderPipeline
        setVertexBuffer passEncoder 0 positionBuffer
        setVertexBuffer passEncoder 1 colorBuffer
        setIndexBuffer passEncoder indexBuffer uint16
        setBindGroup passEncoder 0 uniformBindGroup
        drawIndexedWithInstanceCount passEncoder 36 1
        buf' <- liftST $ STArray.pop outputBuffers
        buf <- flip fromMaybe buf'
          <$> do
            buffer <- createBuffer device $ x
              { size: ((byteLength (Typed.buffer imx)) + 3) .&.
                  complement 3
              , usage: GPUBufferUsage.copyDst .|. GPUBufferUsage.mapRead
              }
            pure buffer
        end passEncoder
        --------
        -- write to output buffer
        -- we use this as a test 
        copyBufferToBuffer commandEncoder perspectiveResultBuffer 0
          buf
          0
          (4 * 16)
        -- 🙌 finish commandEncoder
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
        launchAff_ do
          toAffE $ convertPromise <$> mapAsync buf GPUMapMode.read
          liftEffect do
            mr <- getMappedRange buf
            -- we don't use the mapped range, but we go through the process of
            -- getting it in order to fully test the mapAsync function's timing
            _ <- (whole mr :: Effect Float32Array) >>= toArray
            tnx <- (getTime >>> (_ - startsAt) >>> (_ * 0.001)) <$> now
            cfx <- Ref.read currentFrame
            avgTn <- timeDeltaAverager (tnx - tn)
            avgCf <- frameDeltaAverager (toNumber (cfx - cf))
            setTextContent
              ( "Delta time: " <> show (toStringWith (precision 2) avgTn)
                  <> ", Delta frames: "
                  <> show (toStringWith (precision 2) avgCf)
              )
              (toNode renderStats)
            unmap buf
            void $ liftST $ STArray.push buf outputBuffers
    let
      render = unit # fix \f _ -> do
        -- ⏭ Acquire next image from context
        colorTexture <- getCurrentTexture context
        colorTextureView <- createView colorTexture

        -- 📦 Write and submit commands to queue
        encodeCommands colorTextureView

        -- ➿ Refresh canvas
        window >>= void <<< requestAnimationFrame (f unit)

    liftEffect render
