export const canvasImpl = context => context.canvas;
export const configureImpl = (context, descriptor) =>
  context.configure(descriptor);
export const unconfigureImpl = context => context.unconfigure();
export const getCurrentTextureImpl = context => context.getCurrentTexture();
