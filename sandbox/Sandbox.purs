module Sandbox where

import Prelude

import Control.Lazy (fix)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Typed (class TypedArray, fromArray, setTyped, whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Uint16Array)
import Data.Float32 (Float32)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Int.Bits (complement, (.&.))
import Data.JSDate (getTime, now)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (pi)
import Data.Number as Math
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import GLMatrix.Mat4 as Mat4
import GLMatrix.Vec3 as Vec3
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.BufferSource (fromFloat32Array)
import Web.GPU.GPU (requestAdapter)
import Web.GPU.GPUAdapter (requestDevice)
import Web.GPU.GPUBindGroupEntry (GPUBufferBinding, gpuBindGroupEntry)
import Web.GPU.GPUBindGroupLayoutEntry (gpuBindGroupLayoutEntry)
import Web.GPU.GPUBuffer (GPUBuffer, getMappedRange, unmap)
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
    (setAttribute "style" "display:auto;")

foreign import uggggggh :: Float32Array -> Float32Array

freshIdentityMatrix :: forall t20. TypedArray t20 Float32 => Effect (ArrayView t20)
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

freshTranslateMatrix :: forall t20. TypedArray t20 Float32 => Number -> Number -> Number -> Effect (ArrayView t20)
freshTranslateMatrix x y z = fromArray $ hackyFloatConv
  [ 1.0
  , 0.0
  , 0.0
  , x
  , 0.0
  , 1.0
  , 0.0
  , y
  , 0.0
  , 0.0
  , 1.0
  , z
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

main :: Effect Unit
main = do
  startsAt <- getTime <$> now
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
  let
    makeUniformData t = do
      -- ♟️ ModelViewProjection Matrix (Identity)
      let
        fovy = pi / 2.0
        aspect = 1.0
        f = 1.0 / Math.tan (fovy / 2.0)
        near = 0.1
        far = 10.0
        nf = 1.0 / (near - far)
        perspectivez0 = Mat4.fromValues (f / aspect) 0.0 0.0 0.0 0.0 f 0.0 0.0
          0.0
          0.0
          (far * nf)
          (-1.0)
          0.0
          0.0
          (far * near * nf)
          0.0
        mvp' = Mat4.numbers
          ( perspectivez0 `Mat4.multiply`
              ( flip Mat4.rotateZ (t * 0.5) $
                  ( flip Mat4.rotateY (t * 0.5)
                      $ flip Mat4.rotateX (t * 0.5)
                      $ flip Mat4.translate (Vec3.fromValues 0.0 0.0 (-5.0))
                      $ flip Mat4.scale (Vec3.fromValues 0.25 0.25 0.25)
                      $ Mat4.identity
                  )
              )
          )
        mvp = Mat4.numbers $ flip Mat4.translate (Vec3.fromValues 0.0 0.0 0.0) $
          flip Mat4.scale (Vec3.fromValues 0.25 0.25 0.25) Mat4.identity
      -- (Mat4.ortho (-1.0) (1.0) (-1.0) (1.0) (0.0) (100.0))
      uniformData :: Float32Array <- fromArray $
        hackyFloatConv
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
      pure uniformData
  uniformData <- makeUniformData 0.0
  scaleData :: Float32Array <- freshIdentityMatrix
  timeData :: Float32Array <- fromArray $ hackyFloatConv [ 0.0 ]
  rotateZData :: Float32Array <- freshIdentityMatrix
  rotateZResultData :: Float32Array <- freshIdentityMatrix
  rotateXData :: Float32Array <- freshIdentityMatrix
  rotateXResultData :: Float32Array <- freshIdentityMatrix
  rotateYData :: Float32Array <- freshIdentityMatrix
  rotateYResultData :: Float32Array <- freshIdentityMatrix
  translateZData :: Float32Array <- map identity $  freshTranslateMatrix 0.0 0.0 (-5.0)
  translateZResultData :: Float32Array <- freshIdentityMatrix
  let
    fovy = pi / 2.0
    aspect = 1.0
    f = 1.0 / Math.tan (fovy / 2.0)
    near = 0.1
    far = 10.0
    nf = 1.0 / (near - far)
  perspectiveData :: Float32Array <- map identity $ fromArray $ hackyFloatConv [f / aspect, 0.0, 0.0, 0.0, 0.0, f, 0.0, 0.0,
      0.0,
      0.0,
      far * nf,
      (-1.0),
      0.0,
      0.0,
      far * near * nf,
      0.0]
  perspectiveResultData :: Float32Array <- freshIdentityMatrix
  -- 📇 Index Buffer Data
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

    uniformBuffer <- liftEffect $ createBufferF uniformData
      (GPUBufferUsage.uniform .|. GPUBufferUsage.copyDst)
    timeBuffer <- liftEffect $ createBufferF timeData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copyDst)
    scaleBuffer <- liftEffect $ createBufferF scaleData
      (GPUBufferUsage.storage )
    rotateZBuffer <- liftEffect $ createBufferF rotateZData
      (GPUBufferUsage.storage )
    rotateZResultBuffer <- liftEffect $ createBufferF rotateZResultData
      (GPUBufferUsage.storage)
    rotateXBuffer <- liftEffect $ createBufferF rotateXData
      (GPUBufferUsage.storage )
    rotateXResultBuffer <- liftEffect $ createBufferF rotateXResultData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copySrc)
    rotateYBuffer <- liftEffect $ createBufferF rotateYData
      (GPUBufferUsage.storage )
    rotateYResultBuffer <- liftEffect $ createBufferF rotateYResultData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copySrc)
    translateZBuffer <- liftEffect $ createBufferF translateZData
      (GPUBufferUsage.storage)
    translateZResultBuffer <- liftEffect $ createBufferF translateZResultData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copySrc)
    perspectiveBuffer <- liftEffect $ createBufferF perspectiveData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copySrc)
    perspectiveResultBuffer <- liftEffect $ createBufferF perspectiveResultData
      (GPUBufferUsage.storage .|. GPUBufferUsage.copySrc)
    -- 🖍️ Shaders
    let
      resetDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;

@compute @workgroup_size(4,4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 4u || global_id.y >= 4u) {
    return;
  }

  resultMatrix[global_id.x][global_id.y] = select(0.0, 1.0, global_id.x == global_id.y);
}"""
        }
    resetModule <- liftEffect $ createShaderModule device resetDesc
    let
      initialScaleDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;

@compute @workgroup_size(4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 3u) {
    return;
  }

  resultMatrix[global_id.x][global_id.x] = resultMatrix[global_id.x][global_id.x] * 0.25;
}"""
        }
    initialScaleModule <- liftEffect $ createShaderModule device
      initialScaleDesc
    let
      xTransTestDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;

@compute @workgroup_size(1)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 1u) {
    return; 
  }

  resultMatrix[3][0] = resultMatrix[3][0] + 0.3;
}"""
        }
    xTransTestModule <- liftEffect $ createShaderModule device xTransTestDesc
    let
      rotateZDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159;
  var o = (x << 1) + y;
  return sin(((time / 4.0) * pi) + (f32(2 - ((o + 1) % 3)) * (pi / 2.0)));
}

@compute @workgroup_size(2,2)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 2u || global_id.y >= 2u) {
    return; 
  }
  // 0,0 is cos(a)   0 1
  // 0,1 is sin(a)   1 0
  // 1,0 is -sin(a)  2 2
  // 1,1 is cos(a)   3 1 
  resultMatrix[global_id.y][global_id.x] = xyt2trig(global_id.x, global_id.y, time);
}"""
        }
    rotateZModule <- liftEffect $ createShaderModule device rotateZDesc
    let
      rotateYDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159;
  var o = (x << 1) + y;
  return sin(((time / 4.0) * pi) + (f32(2 - ((o + 1) % 3)) * (pi / 2.0)));
}

@compute @workgroup_size(2,2)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 2u || global_id.y >= 2u) {
    return; 
  }
  // 0,0 is cos(a)   0 1
  // 0,1 is sin(a)   1 0
  // 1,0 is -sin(a)  2 2
  // 1,1 is cos(a)   3 1 
  resultMatrix[global_id.y * 2][global_id.x * 2] = xyt2trig(global_id.x, global_id.y, time);
}"""
        }
    rotateYModule <- liftEffect $ createShaderModule device rotateYDesc
    let
      rotateXDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read_write> resultMatrix : mat4x4<f32>;
@group(1) @binding(0) var<storage, read> time : f32;
fn xyt2trig(x: u32, y: u32, time: f32) -> f32 {
  const pi = 3.14159;
  var o = (x << 1) + y;
  return sin(((time / 4.0) * pi) + (f32((o + 1) % 3) * (pi / 2.0)));
}

@compute @workgroup_size(2,2)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 2u || global_id.y >= 2u) {
    return; 
  }
  // 1,1 is cos(a)   0 1
  // 1,2 is -sin(a)  1 2
  // 2,1 is sin(a)   2 0
  // 2,2 is cos(a)   3 1 
  resultMatrix[global_id.y + 1][global_id.x + 1] = xyt2trig(global_id.x, global_id.y, time);
}"""
        }
    rotateXModule <- liftEffect $ createShaderModule device rotateXDesc
    let
      matrixMultiplicationDesc = x
        { code:
            """
@group(0) @binding(0) var<storage, read> matrixL : mat4x4<f32>;
@group(0) @binding(1) var<storage, read> matrixR : mat4x4<f32>;
@group(0) @binding(2) var<storage, read_write> resultMatrix : mat4x4<f32>;
@compute @workgroup_size(4,4)
fn main(@builtin(global_invocation_id) global_id : vec3<u32>) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= 4u || global_id.y >= 4u) {
    return; 
  }
  var result = 0.0;
  for (var i = 0u; i < 4u; i = i + 1u) {
    result = result + (matrixL[global_id.x][i] * matrixR[i][global_id.y]);
  }

  resultMatrix[global_id.x][global_id.y] = result;
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
    translateZResultIOComputeBindGroup <- liftEffect $ createBindGroup device $ x
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
    perspectiveResultIOComputeBindGroup <- liftEffect $ createBindGroup device $ x
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
      (resetState :: GPUProgrammableStage) = x
        { "module": resetModule
        , entryPoint: "main"
        }
      (initialScaleState :: GPUProgrammableStage) = x
        { "module": initialScaleModule
        , entryPoint: "main"
        }
      (xTransTestState :: GPUProgrammableStage) = x
        { "module": xTransTestModule
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
    resetPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: simpleIOComputeLayout
      , compute: resetState
      }
    initialScalePipeline <- liftEffect $ createComputePipeline device $ x
      { layout: simpleIOComputeLayout
      , compute: initialScaleState
      }
    xTransTestPipeline <- liftEffect $ createComputePipeline device $ x
      { layout: simpleIOComputeLayout
      , compute: xTransTestState
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
      d <- window >>= document
      canvas <-
        ( (_ >>= fromElement) <$> getElementById "gfx"
            (toNonElementParentNode d)
        ) >>= maybe
          (showErrorMessage *> throwError (error "counld not find canvas"))
          pure

      context <- getContext canvas >>= maybe
        (showErrorMessage *> throwError (error "counld not find context"))
        pure
      canvasWidth <- width canvas
      canvasHeight <- height canvas
      pure { context, canvasWidth, canvasHeight }
    let
      (config :: GPUCanvasConfiguration) = x
        { device
        , format: GPUTextureFormat.bgra8unorm
        , usage:
            GPUTextureUsage.renderAttachment .|.
              GPUTextureUsage.copySrc
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
        tn <- getTime <$> now
        timeNowData :: Float32Array <- fromArray $ hackyFloatConv
          [ ((tn - startsAt) / 1000.0) ]

        writeBuffer queue timeBuffer 0 (fromFloat32Array timeNowData)
        GPUComputePassEncoder.setPipeline scalePassEncoder initialScalePipeline
        GPUComputePassEncoder.setBindGroup scalePassEncoder 0
          scaleBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsX scalePassEncoder 4
        GPUComputePassEncoder.end scalePassEncoder
        ---------------------
        rotateZPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateZPassEncoder
          rotateZPipeline
        GPUComputePassEncoder.setBindGroup rotateZPassEncoder 0
          rotateZComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateZPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateZPassEncoder 2 2
        GPUComputePassEncoder.end rotateZPassEncoder
        ---------------------
        rotateZMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateZMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateZMulPassEncoder 0
          rotateZResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateZMulPassEncoder 4 4
        GPUComputePassEncoder.end rotateZMulPassEncoder
        ---------------------
        rotateXPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateXPassEncoder
          rotateXPipeline
        GPUComputePassEncoder.setBindGroup rotateXPassEncoder 0
          rotateXComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateXPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateXPassEncoder 2 2
        GPUComputePassEncoder.end rotateXPassEncoder
        ---------------------
        rotateXMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateXMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateXMulPassEncoder 0
          rotateXResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateXMulPassEncoder 4 4
        GPUComputePassEncoder.end rotateXMulPassEncoder
        ---------------------
        rotateYPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateYPassEncoder
          rotateYPipeline
        GPUComputePassEncoder.setBindGroup rotateYPassEncoder 0
          rotateYComputeBindGroup
        GPUComputePassEncoder.setBindGroup rotateYPassEncoder 1
          timeComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateYPassEncoder 2 2
        GPUComputePassEncoder.end rotateYPassEncoder
        ---------------------
        rotateYMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline rotateYMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup rotateYMulPassEncoder 0
          rotateYResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY rotateYMulPassEncoder 4 4
        GPUComputePassEncoder.end rotateYMulPassEncoder
        ---------------------
        translateZMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline translateZMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup translateZMulPassEncoder 0
          translateZResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY translateZMulPassEncoder 4 4
        GPUComputePassEncoder.end translateZMulPassEncoder
        ---------------------
        perspectiveMulPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline perspectiveMulPassEncoder
          matrixMultiplicationPipeline
        GPUComputePassEncoder.setBindGroup perspectiveMulPassEncoder 0
          perspectiveResultIOComputeBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY perspectiveMulPassEncoder 4 4
        GPUComputePassEncoder.end perspectiveMulPassEncoder


        -- xTransTestPassEncoder <- beginComputePass commandEncoder (x {})
        -- GPUComputePassEncoder.setPipeline xTransTestPassEncoder
        --   xTransTestPipeline
        -- GPUComputePassEncoder.setBindGroup xTransTestPassEncoder 0
        --   simpleIOComputeBindGroup
        -- GPUComputePassEncoder.dispatchWorkgroupsX xTransTestPassEncoder 1
        -- GPUComputePassEncoder.end xTransTestPassEncoder
        --
        copyBufferToBuffer commandEncoder perspectiveResultBuffer 0 uniformBuffer 0
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
        end passEncoder
        -- 💻 Encode compute commands for resetting buffer
        resetPassEncoder <- beginComputePass commandEncoder (x {})
        GPUComputePassEncoder.setPipeline resetPassEncoder resetPipeline
        -- reset the main storage
        GPUComputePassEncoder.setBindGroup resetPassEncoder 0
          scaleBindGroup
        GPUComputePassEncoder.dispatchWorkgroupsXY resetPassEncoder 4 4
        GPUComputePassEncoder.end resetPassEncoder
        -- 🙌 finish commandEncoder
        toSubmit <- finish commandEncoder
        submit queue [ toSubmit ]
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
