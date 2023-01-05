export const setViewportImpl =
  (gpuRenderPassEncoder) =>
  (num$1) =>
  (num$2) =>
  (num$3) =>
  (num$4) =>
  (num$5) =>
  (num$6) =>
  () =>
    gpuRenderPassEncoder.setViewport(num$1, num$2, num$3, num$4, num$5, num$6);
export const setScissorRectImpl =
  (gpuRenderPassEncoder) =>
  (gpuIntegerCoordinate$1) =>
  (gpuIntegerCoordinate$2) =>
  (gpuIntegerCoordinate$3) =>
  (gpuIntegerCoordinate$4) =>
  () =>
    gpuRenderPassEncoder.setScissorRect(
      gpuIntegerCoordinate$1,
      gpuIntegerCoordinate$2,
      gpuIntegerCoordinate$3,
      gpuIntegerCoordinate$4
    );
export const setBlendConstantImpl =
  (gpuRenderPassEncoder) => (gpuColor$1) => () =>
    gpuRenderPassEncoder.setBlendConstant(gpuColor$1);
export const setStencilReferenceImpl =
  (gpuRenderPassEncoder) => (gpuStencilValue$1) => () =>
    gpuRenderPassEncoder.setStencilReference(gpuStencilValue$1);
export const beginOcclusionQueryImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => () =>
    gpuRenderPassEncoder.beginOcclusionQuery(gpuSize32$1);
export const endOcclusionQueryImpl = (gpuRenderPassEncoder) => () =>
  gpuRenderPassEncoder.endOcclusionQuery();
export const executeBundlesImpl =
  (gpuRenderPassEncoder) => (arraygpuRenderBundle$1) => () =>
    gpuRenderPassEncoder.executeBundles(arraygpuRenderBundle$1);
export const endImpl = (gpuRenderPassEncoder) => () =>
  gpuRenderPassEncoder.end();
export const setPipelineImpl =
  (gpuRenderPassEncoder) => (gpuRenderPipeline$1) => () =>
    gpuRenderPassEncoder.setPipeline(gpuRenderPipeline$1);
export const setIndexBufferImpl =
  (gpuRenderPassEncoder) => (gpuBuffer$1) => (gpuIndexFormat$2) => () =>
    gpuRenderPassEncoder.setIndexBuffer(gpuBuffer$1, gpuIndexFormat$2);
export const setIndexBufferWithSizeImpl =
  (gpuRenderPassEncoder) =>
  (gpuBuffer$1) =>
  (gpuIndexFormat$2) =>
  (gpuSize64$3) =>
  () =>
    gpuRenderPassEncoder.setIndexBufferWithSize(
      gpuBuffer$1,
      gpuIndexFormat$2,
      gpuSize64$3
    );
export const setIndexBufferWithOffsetImpl =
  (gpuRenderPassEncoder) =>
  (gpuBuffer$1) =>
  (gpuIndexFormat$2) =>
  (gpuSize64$3) =>
  () =>
    gpuRenderPassEncoder.setIndexBufferWithOffset(
      gpuBuffer$1,
      gpuIndexFormat$2,
      gpuSize64$3
    );
export const setIndexBufferWithOffsetAndSizeImpl =
  (gpuRenderPassEncoder) =>
  (gpuBuffer$1) =>
  (gpuIndexFormat$2) =>
  (gpuSize64$3) =>
  (gpuSize64$4) =>
  () =>
    gpuRenderPassEncoder.setIndexBufferWithOffsetAndSize(
      gpuBuffer$1,
      gpuIndexFormat$2,
      gpuSize64$3,
      gpuSize64$4
    );
export const setVertexBufferImpl =
  (gpuRenderPassEncoder) => (gpuIndex32$1) => (gpuBuffer$2) => () =>
    gpuRenderPassEncoder.setVertexBuffer(gpuIndex32$1, gpuBuffer$2);
export const setVertexBufferWithOffsetImpl =
  (gpuRenderPassEncoder) =>
  (gpuIndex32$1) =>
  (gpuBuffer$2) =>
  (gpuSize64$3) =>
  () =>
    gpuRenderPassEncoder.setVertexBufferWithOffset(
      gpuIndex32$1,
      gpuBuffer$2,
      gpuSize64$3
    );
export const setVertexBufferWithSizeImpl =
  (gpuRenderPassEncoder) =>
  (gpuIndex32$1) =>
  (gpuBuffer$2) =>
  (gpuSize64$3) =>
  () =>
    gpuRenderPassEncoder.setVertexBufferWithSize(
      gpuIndex32$1,
      gpuBuffer$2,
      gpuSize64$3
    );
export const setVertexBufferWithOffsetAndSizeImpl =
  (gpuRenderPassEncoder) =>
  (gpuIndex32$1) =>
  (gpuBuffer$2) =>
  (gpuSize64$3) =>
  (gpuSize64$4) =>
  () =>
    gpuRenderPassEncoder.setVertexBufferWithOffsetAndSize(
      gpuIndex32$1,
      gpuBuffer$2,
      gpuSize64$3,
      gpuSize64$4
    );
export const drawImpl = (gpuRenderPassEncoder) => (gpuSize32$1) => () =>
  gpuRenderPassEncoder.draw(gpuSize32$1);
export const drawWithInstanceCountImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => (gpuSize32$2) => () =>
    gpuRenderPassEncoder.drawWithInstanceCount(gpuSize32$1, gpuSize32$2);
export const drawWithInstanceCountAndFirstVertexImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawWithInstanceCountAndFirstVertex(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawWithInstanceCountAndFirstInstance =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawWithInstanceCountAndFirstInst(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawWithFirstVertexAndFirstInstance =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawWithFirstVertexAndFirstInst(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  (gpuSize32$4) =>
  () =>
    gpuRenderPassEncoder.drawWithInstanceCountAndFirstVertexAndFirstInstance(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3,
      gpuSize32$4
    );
export const drawIndexedImpl = (gpuRenderPassEncoder) => (gpuSize32$1) => () =>
  gpuRenderPassEncoder.drawIndexed(gpuSize32$1);
export const drawIndexedWithInstanceCountImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => (gpuSize32$2) => () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCount(gpuSize32$1, gpuSize32$2);
export const drawIndexedWithFirstIndexImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => (gpuSize32$2) => () =>
    gpuRenderPassEncoder.drawIndexedWithFirstIndex(gpuSize32$1, gpuSize32$2);
export const drawIndexedWithBaseVertexImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => (gpuSignedOffset32$2) => () =>
    gpuRenderPassEncoder.drawIndexedWithBaseVertex(
      gpuSize32$1,
      gpuSignedOffset32$2
    );
export const drawInstancedWithFirstInstanceImpl =
  (gpuRenderPassEncoder) => (gpuSize32$1) => (gpuSize32$2) => () =>
    gpuRenderPassEncoder.drawInstancedWithFirstInstance(
      gpuSize32$1,
      gpuSize32$2
    );
export const drawIndexedWithInstanceCountAndFirstIndexImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCountAndFirstIndex(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawIndexedWithInstanceCountAndBaseVertexImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSignedOffset32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCountAndBaseVertex(
      gpuSize32$1,
      gpuSize32$2,
      gpuSignedOffset32$3
    );
export const drawIndexedWithInstanceCountAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCountAndFirstInstance(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawIndexedWithFirstIndexAndBaseVertexImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSignedOffset32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithFirstIndexAndBaseVertex(
      gpuSize32$1,
      gpuSize32$2,
      gpuSignedOffset32$3
    );
export const drawIndexedWithFirstIndexAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithFirstIndexAndFirstInstance(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawIndexedWithBaseVertexAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSignedOffset32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithBaseVertexAndFirstInstance(
      gpuSize32$1,
      gpuSignedOffset32$2,
      gpuSize32$3
    );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSignedOffset32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex(
      gpuSize32$1,
      gpuSize32$2,
      gpuSignedOffset32$3
    );
export const drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSize32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3
    );
export const drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl =
  (gpuRenderPassEncoder) =>
  (gpuSize32$1) =>
  (gpuSignedOffset32$2) =>
  (gpuSize32$3) =>
  () =>
    gpuRenderPassEncoder.drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance(
      gpuSize32$1,
      gpuSignedOffset32$2,
      gpuSize32$3
    );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl =

    (gpuRenderPassEncoder) =>
    (gpuSize32$1) =>
    (gpuSize32$2) =>
    (gpuSignedOffset32$3) =>
    (gpuSize32$4) =>
    () =>
      gpuRenderPassEncoder.drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance(
        gpuSize32$1,
        gpuSize32$2,
        gpuSignedOffset32$3,
        gpuSize32$4
      );
export const drawIndirectImpl =
  (gpuRenderPassEncoder) => (gpuBuffer$1) => (gpuSize64$2) => () =>
    gpuRenderPassEncoder.drawIndirect(gpuBuffer$1, gpuSize64$2);
export const drawIndexedIndirectImpl =
  (gpuRenderPassEncoder) => (gpuBuffer$1) => (gpuSize64$2) => () =>
    gpuRenderPassEncoder.drawIndexedIndirect(gpuBuffer$1, gpuSize64$2);
export const setBindGroupImpl =
  (gpuRenderPassEncoder) => (gpuIndex32$1) => (gpuBindGroup$2) => () =>
    gpuRenderPassEncoder.setBindGroup(gpuIndex32$1, gpuBindGroup$2);
export const setBindGroupWithDynamicOffsetsImpl =
  (gpuRenderPassEncoder) =>
  (gpuIndex32$1) =>
  (gpuBindGroup$2) =>
  (arraygpuBufferDynamicOffset$3) =>
  () =>
    gpuRenderPassEncoder.setBindGroupWithDynamicOffsets(
      gpuIndex32$1,
      gpuBindGroup$2,
      arraygpuBufferDynamicOffset$3
    );
export const setBindGroupWithDyanmicOffsetBounds =
  (gpuRenderPassEncoder) =>
  (gpuIndex32$1) =>
  (gpuBindGroup$2) =>
  (uint32array$3) =>
  (gpuSize64$4) =>
  (gpuSize32$5) =>
  () =>
    gpuRenderPassEncoder.setBindGroupWithDyanmicOffsetBo(
      gpuIndex32$1,
      gpuBindGroup$2,
      uint32array$3,
      gpuSize64$4,
      gpuSize32$5
    );
export const pushDebugGroupImpl = (gpuRenderPassEncoder) => (str$1) => () =>
  gpuRenderPassEncoder.pushDebugGroup(str$1);
export const popDebugGroupImpl = (gpuRenderPassEncoder) => () =>
  gpuRenderPassEncoder.popDebugGroup();
export const insertDebugMarkerImpl = (gpuRenderPassEncoder) => (str$1) => () =>
  gpuRenderPassEncoder.insertDebugMarker(str$1);
