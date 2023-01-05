module Sandbox where

import Prelude

import Control.Lazy (fix)
import Control.Promise (toAffE)
import Control.Promise as Control.Promise
import Data.ArrayBuffer.ArrayBuffer (byteLength)
import Data.ArrayBuffer.Typed (class TypedArray, fromArray, setTyped, whole)
import Data.ArrayBuffer.Typed as Typed
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Uint16Array)
import Data.Float32 (fromNumber')
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Int.Bits (complement, (.&.))
import Data.Maybe (Maybe(..), maybe)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.GPU.Device (createBuffer, createCommandEncoder, createPipelineLayout, createRenderPipeline, createShaderModule, createTexture)
import Web.GPU.Device as GPUDevice
import Web.GPU.GPU (requestAdapter)
import Web.GPU.GPUAdapter (requestDevice)
import Web.GPU.GPUBuffer (getMappedRange, unmap)
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.GPUBufferUsage as GPUBufferUsage
import Web.GPU.GPUCanvasAlphaMode (opaque)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUCanvasContext (configure, getCurrentTexture)
import Web.GPU.GPUColor (gpuColorDict)
import Web.GPU.GPUColorTargetState (GPUColorTargetState)
import Web.GPU.GPUCommandEncoder (beginRenderPass, finish)
import Web.GPU.GPUCompareFunction as GPUCompareFunction
import Web.GPU.GPUCullMode (none)
import Web.GPU.GPUDepthStencilState (GPUDepthStencilState)
import Web.GPU.GPUExtent3D (gpuExtent3DWHD)
import Web.GPU.GPUFragmentState (GPUFragmentState)
import Web.GPU.GPUFrontFace (cw)
import Web.GPU.GPUIndexFormat (uint16)
import Web.GPU.GPULoadOp as GPULoadOp
import Web.GPU.GPUPrimitiveState (GPUPrimitiveState)
import Web.GPU.GPUPrimitiveTopology (triangleList)
import Web.GPU.GPUQueue (submit)
import Web.GPU.GPURenderPassColorAttachment (GPURenderPassColorAttachment)
import Web.GPU.GPURenderPassDepthStencilAttachment (GPURenderPassDepthStencilAttachment)
import Web.GPU.GPURenderPassDescriptor (GPURenderPassDescriptor)
import Web.GPU.GPURenderPassEncoder (drawIndexedWithInstanceCount, end, setIndexBuffer, setPipeline, setScissorRect, setVertexBuffer, setViewport)
import Web.GPU.GPURenderPipelineDescriptor (GPURenderPipelineDescriptor)
import Web.GPU.GPUStoreOp as GPUStoreOp
import Web.GPU.GPUTexture (createView)
import Web.GPU.GPUTextureDescriptor (GPUTextureDescriptor)
import Web.GPU.GPUTextureDimension as GPUTextureDimension
import Web.GPU.GPUTextureFormat (bgra8unorm, depth24plusStencil8)
import Web.GPU.GPUTextureUsage as GPUTextureUsage
import Web.GPU.GPUVertexAttribute (GPUVertexAttribute)
import Web.GPU.GPUVertexBufferLayout (GPUVertexBufferLayout)
import Web.GPU.GPUVertexFormat (float32x3)
import Web.GPU.GPUVertexState (GPUVertexState)
import Web.GPU.GPUVertexStepMode as StepMode
import Web.GPU.HTMLCanvasElement (getContext)
import Web.GPU.Internal.Bitwise ((.|.))
import Web.GPU.Internal.RequiredAndOptional (e, (~))
import Web.GPU.Internal.Types (GPUBuffer)
import Web.GPU.Navigator (gpu)
import Web.HTML (window)
import Web.HTML.HTMLCanvasElement (fromElement, height, width)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, navigator, requestAnimationFrame)
import Web.Promise as Web.Promise

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
  positions :: Float32Array <- fromArray $ map fromNumber'
    [ 1.0
    , -1.0
    , 0.0
    , -1.0
    , -1.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    ]

  -- 🎨 Color Vertex Buffer Data
  colors :: Float32Array <- fromArray $ map fromNumber'
    [ 1.0
    , 0.0
    , 0.0
    , -- 🔴
      0.0
    , 1.0
    , 0.0
    , -- 🟢
      0.0
    , 0.0
    , 1.0
    -- 🔵
    ]
  -- 📇 Index Buffer Data
  indices :: Uint16Array <- fromArray $ map fromInt [ 0, 1, 2 ]
  -- 🏭 Entry to WebGPU
  entry <- window >>= navigator >>= gpu >>= case _ of
    Nothing -> do
      showErrorMessage
      throwError $ error "WebGPU is not supported"
    Just entry -> pure entry
  launchAff_ do
    adapter <- (toAffE $ convertPromise <$> requestAdapter entry e) >>=
      case _ of
        Nothing -> liftEffect do
          showErrorMessage
          throwError $ error "WebGPU is not supported"
        Just adapter -> pure adapter
    device <- (toAffE $ convertPromise <$> requestDevice adapter e) >>=
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
          desc =
            { size: ((byteLength (Typed.buffer arr)) + 3) .&. complement 3
            , usage
            } ~ { mappedAtCreation: true }
        buffer <- createBuffer device desc
        writeArray <- getMappedRange buffer >>= whole
        _ <- setTyped writeArray Nothing arr
        unmap buffer
        pure buffer
    positionBuffer <- liftEffect $ createBufferF positions GPUBufferUsage.vertex
    colorBuffer <- liftEffect $ createBufferF colors GPUBufferUsage.vertex
    indexBuffer <- liftEffect $ createBufferF indices GPUBufferUsage.index
    -- 🖍️ Shaders
    let
      vsmDesc =
        { code:
            """
struct VSOut {
    @builtin(position) Position: vec4<f32>,
    @location(0) color: vec3<f32>,
};

@vertex
fn main(@location(0) inPos: vec3<f32>,
        @location(1) inColor: vec3<f32>) -> VSOut {
    var vsOut: VSOut;
    vsOut.Position = vec4<f32>(inPos, 1.0);
    vsOut.color = inColor;
    return vsOut;
}
"""
        } ~ {}
    vertModule <- liftEffect $ createShaderModule device vsmDesc
    let
      fsmDesc =
        { code:
            """
@fragment
fn main(@location(0) inColor: vec3<f32>) -> @location(0) vec4<f32> {
    return vec4<f32>(inColor, 1.0);
}
"""
        } ~ {}
    fragModule <- liftEffect $ createShaderModule device fsmDesc

    -- ⚗️ Graphics Pipeline

    -- 🔣 Input Assembly
    let
      (positionAttribDesc :: GPUVertexAttribute) =
        { shaderLocation: 0
        , -- [[location(0)]]
          offset: 0
        , format: float32x3
        } ~ {}
    let
      (colorAttribDesc :: GPUVertexAttribute) =
        { shaderLocation: 1
        , -- [[location(1)]]
          offset: 0
        , format: float32x3
        } ~ {}
    let
      (positionBufferDesc :: GPUVertexBufferLayout) =
        { attributes: [ positionAttribDesc ]
        , arrayStride: 4 * 3
        -- sizeof(float) * 3
        } ~ { stepMode: StepMode.vertex }
    let
      (colorBufferDesc :: GPUVertexBufferLayout) =
        { attributes: [ colorAttribDesc ]
        , arrayStride: 4 * 3
        -- sizeof(float) * 3
        } ~ { stepMode: StepMode.vertex }

    -- 🌑 Depth
    let
      (depthStencil :: GPUDepthStencilState) =
        { format: depth24plusStencil8
        } ~
          { depthWriteEnabled: true
          , depthCompare: GPUCompareFunction.less
          }

    -- 🦄 Uniform Data
    let pipelineLayoutDesc = { bindGroupLayouts: [] } ~ {}
    layout <- liftEffect $ createPipelineLayout device pipelineLayoutDesc

    -- 🎭 Shader Stages
    let
      (vertex :: GPUVertexState) =
        { "module": vertModule
        , entryPoint: "main"
        } ~ { buffers: [ positionBufferDesc, colorBufferDesc ] }

    -- 🌀 Color/Blend State
    let
      (colorState :: GPUColorTargetState) =
        { format: bgra8unorm
        } ~ {}

    let
      (fragment :: GPUFragmentState) =
        { "module": fragModule
        , entryPoint: "main"
        , targets: [ colorState ]
        } ~ {}

    -- 🟨 Rasterization
    let
      (primitive :: GPUPrimitiveState) =
        {} ~
          { frontFace: cw
          , cullMode: none
          , topology: triangleList
          }

    let
      (pipelineDesc :: GPURenderPipelineDescriptor) =
        { layout
        , vertex
        } ~
          { fragment
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
      (config :: GPUCanvasConfiguration) =
        { device
        , format: bgra8unorm
        } ~
          { usage:
              GPUTextureUsage.renderAttachment .|.
                GPUTextureUsage.copySrc
          , alphaMode: opaque
          }
    liftEffect $ configure context config
    let
      (depthTextureDesc :: GPUTextureDescriptor) =
        { size: gpuExtent3DWHD canvasWidth canvasHeight 1
        , format: depth24plusStencil8
        , usage: GPUTextureUsage.renderAttachment .|. GPUTextureUsage.copySrc
        } ~
          { dimension: GPUTextureDimension.twoD
          }
    depthTexture <- liftEffect $ createTexture device depthTextureDesc
    depthTextureView <- liftEffect $ createView depthTexture
    let
      encodeCommands colorTextureView = do
        let
          (colorAttachment :: GPURenderPassColorAttachment) =
            { view: colorTextureView
            , loadOp: GPULoadOp.clear
            , storeOp: GPUStoreOp.store
            } ~
              { clearValue: gpuColorDict { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
              }

        let
          (depthAttachment :: GPURenderPassDepthStencilAttachment) =
            { view: depthTextureView

            } ~
              { depthClearValue: 1.0
              , depthLoadOp: GPULoadOp.clear
              , depthStoreOp: GPUStoreOp.store
              , stencilClearValue: 0
              , stencilLoadOp: GPULoadOp.clear
              , stencilStoreOp: GPUStoreOp.store
              }

        let
          (renderPassDesc :: GPURenderPassDescriptor) =
            { colorAttachments: [ colorAttachment ]
            } ~ { depthStencilAttachment: depthAttachment }

        commandEncoder <- createCommandEncoder device e

        -- 🖌️ Encode drawing commands
        passEncoder <- beginRenderPass commandEncoder renderPassDesc
        setPipeline passEncoder pipeline
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
        setVertexBuffer passEncoder 0 positionBuffer
        setVertexBuffer passEncoder 1 colorBuffer
        setIndexBuffer passEncoder indexBuffer uint16
        drawIndexedWithInstanceCount passEncoder 3 1
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
