export const copySrc = window.GPUTextureUsage
  ? window.GPUTextureUsage.COPY_SRC
  : 0x0000;
export const copyDst = window.GPUTextureUsage
  ? window.GPUTextureUsage.COPY_DST
  : 0x0000;
export const textureBinding = window.GPUTextureUsage
  ? window.GPUTextureUsage.TEXTURE_BINDING
  : 0x0000;
export const storageBinding = window.GPUTextureUsage
  ? window.GPUTextureUsage.STORAGE_BINDING
  : 0x0000;
export const renderAttachment = window.GPUTextureUsage
  ? window.GPUTextureUsage.RENDER_ATTACHMENT
  : 0x0000;
