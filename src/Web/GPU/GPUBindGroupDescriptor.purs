module Web.GPU.GPUBindGroupDescriptor where



import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.GPUBindGroupEntry (GPUBufferBinding)
import Web.GPU.Internal.Types (Binding, GPUBindGroupDescriptor, GPUBindGroupEntries, GPUBindGroupLayout, GPUBufferBindingLayout, GPUExternalTexture, GPUExternalTextureBindingLayout, GPUSampler, GPUSamplerBindingLayout, GPUStorageTextureBindingLayout, GPUTextureBindingLayout, GPUTextureView)

class HarmonizeLayoutAndBindGroupEntries (layoutEntries :: RowList Binding) (bindGroupEntries :: RowList Type) | layoutEntries -> bindGroupEntries

class HarmonizeLayoutAndBindGroupEntry (layout :: Binding) (bindGroup :: Type) | layout -> bindGroup

instance HarmonizeLayoutAndBindGroupEntry GPUBufferBindingLayout GPUBufferBinding
instance HarmonizeLayoutAndBindGroupEntry GPUSamplerBindingLayout GPUSampler
instance HarmonizeLayoutAndBindGroupEntry GPUTextureBindingLayout GPUTextureView
instance HarmonizeLayoutAndBindGroupEntry GPUStorageTextureBindingLayout GPUTextureView
instance HarmonizeLayoutAndBindGroupEntry GPUExternalTextureBindingLayout GPUExternalTexture

instance HarmonizeLayoutAndBindGroupEntries Nil Nil
instance (HarmonizeLayoutAndBindGroupEntry val0 val1, HarmonizeLayoutAndBindGroupEntries rest0 rest1) => HarmonizeLayoutAndBindGroupEntries (Cons key val0 rest0) (Cons key val1 rest1)

bindGroupDescriptor
  :: forall layoutEntries layoutEntriesRL bindGroupEntries bindGroupEntriesRL
   . RowToList layoutEntries layoutEntriesRL
  => RowToList bindGroupEntries bindGroupEntriesRL
  => HarmonizeLayoutAndBindGroupEntries layoutEntriesRL bindGroupEntriesRL
  => GPUBindGroupLayout layoutEntries
  -> GPUBindGroupEntries bindGroupEntries
  -> GPUBindGroupDescriptor layoutEntries bindGroupEntries
bindGroupDescriptor layout entries = unsafeCoerce { layout, entries }