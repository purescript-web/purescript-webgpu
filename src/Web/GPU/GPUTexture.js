export const createViewImpl = texture => texture.createView();
export const createViewWithDescriptorImpl = (texture, descriptor) =>
  texture.createView(descriptor);
export const destroyImpl = texture => texture.destroy();
export const widthImpl = texture => texture.width;
export const heightImpl = texture => texture.height;
export const depthOrArrayLayersImpl = texture => texture.depthOrArrayLayers;
export const mipLevelCountImpl = texture => texture.mipLevelCount;
export const sampleCountImpl = texture => texture.sampleCount;
export const dimensionImpl = texture => texture.dimension;
export const formatImpl = texture => texture.format;
export const usageImpl = texture => texture.usage;
