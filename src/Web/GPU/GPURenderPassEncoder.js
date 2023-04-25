export const setViewportImpl = (
  gpuRenderPassEncoder,
  num$1,
  num$2,
  num$3,
  num$4,
  num$5,
  num$6
) => gpuRenderPassEncoder.setViewport(num$1, num$2, num$3, num$4, num$5, num$6);
export const setScissorRectImpl = (
  gpuRenderPassEncoder,
  gpuIntegerCoordinate$1,
  gpuIntegerCoordinate$2,
  gpuIntegerCoordinate$3,
  gpuIntegerCoordinate$4
) =>
  gpuRenderPassEncoder.setScissorRect(
    gpuIntegerCoordinate$1,
    gpuIntegerCoordinate$2,
    gpuIntegerCoordinate$3,
    gpuIntegerCoordinate$4
  );
export const setBlendConstantImpl = (gpuRenderPassEncoder, gpuColor$1) =>
  gpuRenderPassEncoder.setBlendConstant(gpuColor$1);
export const setStencilReferenceImpl = (
  gpuRenderPassEncoder,
  gpuStencilValue$1
) => gpuRenderPassEncoder.setStencilReference(gpuStencilValue$1);
export const beginOcclusionQueryImpl = (gpuRenderPassEncoder, gpuSize32$1) =>
  gpuRenderPassEncoder.beginOcclusionQuery(gpuSize32$1);
export const endOcclusionQueryImpl = gpuRenderPassEncoder =>
  gpuRenderPassEncoder.endOcclusionQuery();
export const executeBundlesImpl = (
  gpuRenderPassEncoder,
  arraygpuRenderBundle$1
) => gpuRenderPassEncoder.executeBundles(arraygpuRenderBundle$1);
export const endImpl = gpuRenderPassEncoder => gpuRenderPassEncoder.end();
export const setPipelineImpl = (gpuRenderPassEncoder, gpuRenderPipeline$1) =>
  gpuRenderPassEncoder.setPipeline(gpuRenderPipeline$1);
export const setIndexBufferImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2
) => gpuRenderPassEncoder.setIndexBuffer(gpuBuffer$1, gpuIndexFormat$2);
export const setIndexBufferWithSizeImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3
) =>
  gpuRenderPassEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    gpuSize64$3
  );
export const setIndexBufferWithOffsetImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3
) =>
  gpuRenderPassEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    undefined,
    gpuSize64$3
  );
export const setIndexBufferWithOffsetAndSizeImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3,
  gpuSize64$4
) =>
  gpuRenderPassEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    gpuSize64$3,
    gpuSize64$4
  );
export const setVertexBufferImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBuffer$2
) => gpuRenderPassEncoder.setVertexBuffer(gpuIndex32$1, gpuBuffer$2);
export const setVertexBufferWithOffsetImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3
) =>
  gpuRenderPassEncoder.setVertexBuffer(gpuIndex32$1, gpuBuffer$2, gpuSize64$3);
export const setVertexBufferWithSizeImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3
) =>
  gpuRenderPassEncoder.setVertexBuffer(
    gpuIndex32$1,
    gpuBuffer$2,
    undefined,
    gpuSize64$3
  );
export const setVertexBufferWithOffsetAndSizeImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3,
  gpuSize64$4
) =>
  gpuRenderPassEncoder.setVertexBuffer(
    gpuIndex32$1,
    gpuBuffer$2,
    gpuSize64$3,
    gpuSize64$4
  );
export const drawImpl = (gpuRenderPassEncoder, gpuSize32$1) =>
  gpuRenderPassEncoder.draw(gpuSize32$1);
export const drawWithInstanceCountImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderPassEncoder.draw(gpuSize32$1, gpuSize32$2);
export const drawWithFirstVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderPassEncoder.draw(gpuSize32$1, undefined, gpuSize32$2);
export const drawWithFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderPassEncoder.draw(gpuSize32$1, undefined, undefined, gpuSize32$2);
export const drawWithInstanceCountAndFirstVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) => gpuRenderPassEncoder.draw(gpuSize32$1, gpuSize32$2, gpuSize32$3);
export const drawWithInstanceCountAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderPassEncoder.draw(gpuSize32$1, gpuSize32$2, undefined, gpuSize32$3);
export const drawWithFirstVertexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderPassEncoder.draw(gpuSize32$1, undefined, gpuSize32$2, gpuSize32$3);
export const drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSize32$4
) =>
  gpuRenderPassEncoder.draw(gpuSize32$1, gpuSize32$2, gpuSize32$3, gpuSize32$4);
//
export const drawIndexedImpl = (gpuRenderPassEncoder, gpuSize32$1) =>
  gpuRenderPassEncoder.drawIndexed(gpuSize32$1);
export const drawIndexedWithInstanceCountImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderPassEncoder.drawIndexed(gpuSize32$1, gpuSize32$2);
export const drawIndexedWithFirstIndexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderPassEncoder.drawIndexed(gpuSize32$1, undefined, gpuSize32$2);
export const drawIndexedWithBaseVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSignedOffset32$2
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    gpuSignedOffset32$2
  );
export const drawIndexedWithFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    undefined,
    gpuSize32$2
  );
export const drawIndexedWithInstanceCountAndFirstIndexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) => gpuRenderPassEncoder.drawIndexed(gpuSize32$1, gpuSize32$2, gpuSize32$3);
export const drawIndexedWithInstanceCountAndBaseVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    undefined,
    gpuSignedOffset32$3
  );
export const drawIndexedWithInstanceCountAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    undefined,
    undefined,
    gpuSize32$3
  );
export const drawIndexedWithFirstIndexAndBaseVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    gpuSignedOffset32$3
  );
export const drawIndexedWithFirstIndexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    undefined,
    gpuSize32$3
  );
export const drawIndexedWithBaseVertexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSignedOffset32$2,
  gpuSize32$3
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    gpuSignedOffset32$2,
    gpuSize32$3
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSignedOffset32$4
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    gpuSignedOffset32$4
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSize32$4
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    undefined,
    gpuSize32$4
  );
export const drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl = (
  gpuRenderPassEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3,
  gpuSize32$4
) =>
  gpuRenderPassEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    gpuSignedOffset32$3,
    gpuSize32$4
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl =
  (
    gpuRenderPassEncoder,
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    gpuSignedOffset32$4,
    gpuSize32$5
  ) =>
    gpuRenderPassEncoder.drawIndexed(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3,
      gpuSignedOffset32$4,
      gpuSize32$5
    );
//
export const drawIndirectImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuSize64$2
) => gpuRenderPassEncoder.drawIndirect(gpuBuffer$1, gpuSize64$2);
export const drawIndexedIndirectImpl = (
  gpuRenderPassEncoder,
  gpuBuffer$1,
  gpuSize64$2
) => gpuRenderPassEncoder.drawIndexedIndirect(gpuBuffer$1, gpuSize64$2);
export const setBindGroupImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBindGroup$2
) => gpuRenderPassEncoder.setBindGroup(gpuIndex32$1, gpuBindGroup$2);
export const setBindGroupWithDynamicOffsetsImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBindGroup$2,
  arraygpuBufferDynamicOffset$3
) =>
  gpuRenderPassEncoder.setBindGroup(
    gpuIndex32$1,
    gpuBindGroup$2,
    arraygpuBufferDynamicOffset$3
  );
export const setBindGroupWithDyanmicOffsetBoundsImpl = (
  gpuRenderPassEncoder,
  gpuIndex32$1,
  gpuBindGroup$2,
  uint32array$3,
  gpuSize64$4,
  gpuSize32$5
) =>
  gpuRenderPassEncoder.setBindGroup(
    gpuIndex32$1,
    gpuBindGroup$2,
    uint32array$3,
    gpuSize64$4,
    gpuSize32$5
  );
export const pushDebugGroupImpl = (gpuRenderPassEncoder, str$1) =>
  gpuRenderPassEncoder.pushDebugGroup(str$1);
export const popDebugGroupImpl = gpuRenderPassEncoder =>
  gpuRenderPassEncoder.popDebugGroup();
export const insertDebugMarkerImpl = (gpuRenderPassEncoder, str$1) =>
  gpuRenderPassEncoder.insertDebugMarker(str$1);
