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
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUColor (gpuColorDict)
import Web.GPU.GPUColorTargetState (GPUColorTargetState)
import Web.GPU.GPUCommandEncoder (beginRenderPass, finish)
import Web.GPU.GPUCompareFunction as GPUCompareFunction
import Web.GPU.GPUCullMode as GPUCullMode
import Web.GPU.GPUDepthStencilState (GPUDepthStencilState)
import Web.GPU.GPUDevice (createBuffer, createBindGroupLayout, createBindGroup, createCommandEncoder, createPipelineLayout, createRenderPipeline, createShaderModule, createTexture)
import Web.GPU.GPUDevice as GPUDevice
import Web.GPU.GPUExtent3D (gpuExtent3DWHD)
import Web.GPU.GPUFragmentState (GPUFragmentState)
import Web.GPU.GPUFrontFace (cw)
import Web.GPU.GPUIndexFormat (uint16)
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUPrimitiveState (GPUPrimitiveState)
import Web.GPU.GPUPrimitiveTopology (triangleList)
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

main :: Effect Unit
main = do
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
        mvp = Mat4.numbers
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
      -- (Mat4.ortho (-1.0) (1.0) (-1.0) (1.0) (0.0) (100.0))
      uniformData :: Float32Array <- fromArray $
        hackyFloatConv
          mvp <> hackyFloatConv
          [
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

    -- 🖍️ Shaders
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

    uniformBindGroupLayout <- liftEffect $ createBindGroupLayout device $ x
      { entries:
          [ gpuBindGroupLayoutEntry 0 GPUShaderStage.vertex
              (x {} :: GPUBufferBindingLayout)
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
    -- 🦄 Uniform Data
    let pipelineLayoutDesc = x { bindGroupLayouts: [ uniformBindGroupLayout ] }
    layout <- liftEffect $ createPipelineLayout device pipelineLayoutDesc

    -- 🎭 Shader Stages
    let
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
        { layout
        , vertex
        , fragment
        , primitive
        , depthStencil
        }
    pipeline <- liftEffect $ createRenderPipeline device pipelineDesc
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
            -- , stencilClearValue: 0
            --, stencilLoadOp: GPULoadOp.clear
            --, stencilStoreOp: GPUStoreOp.store
            }

        let
          (renderPassDesc :: GPURenderPassDescriptor) = x
            { colorAttachments: [ colorAttachment ]
            , depthStencilAttachment: depthAttachment
            }

        commandEncoder <- createCommandEncoder device (x {})

        -- 🖌️ Encode drawing commands
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
        setPipeline passEncoder pipeline
        setVertexBuffer passEncoder 0 positionBuffer
        setVertexBuffer passEncoder 1 colorBuffer
        setIndexBuffer passEncoder indexBuffer uint16
        tn <- getTime <$> now
        newBuffer <- makeUniformData (tn * 0.001)
        writeBuffer queue uniformBuffer 0 (fromFloat32Array newBuffer)
        setBindGroup passEncoder 0 uniformBindGroup
        drawIndexedWithInstanceCount passEncoder 36 1
        end passEncoder
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
