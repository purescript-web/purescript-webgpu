export const finishImpl = (gpuRenderBundleEncoder, descriptor) =>
  gpuRenderBundleEncoder.finish(descriptor);
export const setPipelineImpl = (gpuRenderBundleEncoder, gpuRenderPipeline$1) =>
  gpuRenderBundleEncoder.setPipeline(gpuRenderPipeline$1);
export const setIndexBufferImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2
) => gpuRenderBundleEncoder.setIndexBuffer(gpuBuffer$1, gpuIndexFormat$2);
export const setIndexBufferWithSizeImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3
) =>
  gpuRenderBundleEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    gpuSize64$3
  );
export const setIndexBufferWithOffsetImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3
) =>
  gpuRenderBundleEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    undefined,
    gpuSize64$3
  );
export const setIndexBufferWithOffsetAndSizeImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuIndexFormat$2,
  gpuSize64$3,
  gpuSize64$4
) =>
  gpuRenderBundleEncoder.setIndexBuffer(
    gpuBuffer$1,
    gpuIndexFormat$2,
    gpuSize64$3,
    gpuSize64$4
  );
export const setVertexBufferImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBuffer$2
) => gpuRenderBundleEncoder.setVertexBuffer(gpuIndex32$1, gpuBuffer$2);
export const setVertexBufferWithOffsetImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3
) =>
  gpuRenderBundleEncoder.setVertexBuffer(
    gpuIndex32$1,
    gpuBuffer$2,
    gpuSize64$3
  );
export const setVertexBufferWithSizeImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3
) =>
  gpuRenderBundleEncoder.setVertexBuffer(
    gpuIndex32$1,
    gpuBuffer$2,
    undefined,
    gpuSize64$3
  );
export const setVertexBufferWithOffsetAndSizeImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBuffer$2,
  gpuSize64$3,
  gpuSize64$4
) =>
  gpuRenderBundleEncoder.setVertexBuffer(
    gpuIndex32$1,
    gpuBuffer$2,
    gpuSize64$3,
    gpuSize64$4
  );
export const drawImpl = (gpuRenderBundleEncoder, gpuSize32$1) =>
  gpuRenderBundleEncoder.draw(gpuSize32$1);
export const drawWithInstanceCountImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderBundleEncoder.draw(gpuSize32$1, gpuSize32$2);
export const drawWithFirstVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderBundleEncoder.draw(gpuSize32$1, undefined, gpuSize32$2);
export const drawWithFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) =>
  gpuRenderBundleEncoder.draw(gpuSize32$1, undefined, undefined, gpuSize32$2);
export const drawWithInstanceCountAndFirstVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) => gpuRenderBundleEncoder.draw(gpuSize32$1, gpuSize32$2, gpuSize32$3);
export const drawWithInstanceCountAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderBundleEncoder.draw(gpuSize32$1, gpuSize32$2, undefined, gpuSize32$3);
export const drawWithFirstVertexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderBundleEncoder.draw(gpuSize32$1, undefined, gpuSize32$2, gpuSize32$3);
export const drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSize32$4
) =>
  gpuRenderBundleEncoder.draw(
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    gpuSize32$4
  );
//
export const drawIndexedImpl = (gpuRenderBundleEncoder, gpuSize32$1) =>
  gpuRenderBundleEncoder.drawIndexed(gpuSize32$1);
export const drawIndexedWithInstanceCountImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderBundleEncoder.drawIndexed(gpuSize32$1, gpuSize32$2);
export const drawIndexedWithFirstIndexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) => gpuRenderBundleEncoder.drawIndexed(gpuSize32$1, undefined, gpuSize32$2);
export const drawIndexedWithBaseVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSignedOffset32$2
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    gpuSignedOffset32$2
  );
export const drawIndexedWithFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    undefined,
    gpuSize32$2
  );
export const drawIndexedWithInstanceCountAndFirstIndexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) => gpuRenderBundleEncoder.drawIndexed(gpuSize32$1, gpuSize32$2, gpuSize32$3);
export const drawIndexedWithInstanceCountAndBaseVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    undefined,
    gpuSignedOffset32$3
  );
export const drawIndexedWithInstanceCountAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    undefined,
    undefined,
    gpuSize32$3
  );
export const drawIndexedWithFirstIndexAndBaseVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    gpuSignedOffset32$3
  );
export const drawIndexedWithFirstIndexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    undefined,
    gpuSize32$3
  );
export const drawIndexedWithBaseVertexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSignedOffset32$2,
  gpuSize32$3
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    undefined,
    gpuSignedOffset32$2,
    gpuSize32$3
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSignedOffset32$4
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    gpuSignedOffset32$4
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSize32$3,
  gpuSize32$4
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    undefined,
    gpuSize32$4
  );
export const drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl = (
  gpuRenderBundleEncoder,
  gpuSize32$1,
  gpuSize32$2,
  gpuSignedOffset32$3,
  gpuSize32$4
) =>
  gpuRenderBundleEncoder.drawIndexed(
    gpuSize32$1,
    undefined,
    gpuSize32$2,
    gpuSignedOffset32$3,
    gpuSize32$4
  );
export const drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl =
  (
    gpuRenderBundleEncoder,
    gpuSize32$1,
    gpuSize32$2,
    gpuSize32$3,
    gpuSignedOffset32$4,
    gpuSize32$5
  ) =>
    gpuRenderBundleEncoder.drawIndexed(
      gpuSize32$1,
      gpuSize32$2,
      gpuSize32$3,
      gpuSignedOffset32$4,
      gpuSize32$5
    );
//
export const drawIndirectImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuSize64$2
) => gpuRenderBundleEncoder.drawIndirect(gpuBuffer$1, gpuSize64$2);
export const drawIndexedIndirectImpl = (
  gpuRenderBundleEncoder,
  gpuBuffer$1,
  gpuSize64$2
) => gpuRenderBundleEncoder.drawIndexedIndirect(gpuBuffer$1, gpuSize64$2);
export const setBindGroupImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBindGroup$2
) => gpuRenderBundleEncoder.setBindGroup(gpuIndex32$1, gpuBindGroup$2);
export const setBindGroupWithDynamicOffsetsImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBindGroup$2,
  arraygpuBufferDynamicOffset$3
) =>
  gpuRenderBundleEncoder.setBindGroup(
    gpuIndex32$1,
    gpuBindGroup$2,
    arraygpuBufferDynamicOffset$3
  );
export const setBindGroupWithDyanmicOffsetBoundsImpl = (
  gpuRenderBundleEncoder,
  gpuIndex32$1,
  gpuBindGroup$2,
  uint32array$3,
  gpuSize64$4,
  gpuSize32$5
) =>
  gpuRenderBundleEncoder.setBindGroup(
    gpuIndex32$1,
    gpuBindGroup$2,
    uint32array$3,
    gpuSize64$4,
    gpuSize32$5
  );
export const pushDebugGroupImpl = (gpuRenderBundleEncoder, str$1) =>
  gpuRenderBundleEncoder.pushDebugGroup(str$1);
export const popDebugGroupImpl = gpuRenderBundleEncoder =>
  gpuRenderBundleEncoder.popDebugGroup();
export const insertDebugMarkerImpl = (gpuRenderBundleEncoder, str$1) =>
  gpuRenderBundleEncoder.insertDebugMarker(str$1);
