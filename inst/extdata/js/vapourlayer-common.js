var GD = null;
initGdalJs({ path: 'https://cdn.jsdelivr.net/npm/gdal3.js@2.8.1/dist/package', useWorker: false }).then((Gdal) => { GD = Gdal; });

async function reproj_extent(ex, s_crs, t_crs) {
    const exy = await GD.gdaltransform([[ex[0], ex[2]], [ex[1], ex[2]], [ex[0], ex[3]], [ex[1], ex[3]]], ["-s_srs", s_crs, "-t_srs", t_crs, "-output_xy"]);
    return [Math.min(exy[0][0], exy[1][0], exy[2][0], exy[3][0]), Math.max(exy[0][0], exy[1][0], exy[2][0], exy[3][0]),
            Math.min(exy[0][1], exy[1][1], exy[2][1], exy[3][1]), Math.max(exy[0][1], exy[1][1], exy[2][1], exy[3][1])];
}

const is_canvas_blank = function(cvs) {
    const context = cvs.getContext('2d');
    const pixelBuffer = new Uint32Array(
        context.getImageData(0, 0, cvs.width, cvs.height).data.buffer
    );
    return !pixelBuffer.some(color => color !== 0);
}

const px2m = function(xy, xsc, xoff, ysc, yoff, imxoff, imyoff, imh) { return [(xy[0] - imxoff) * xsc + xoff, ((imh - xy[1]) - imyoff) * ysc + yoff]; }
const m2px = function(xy, xsc, xoff, ysc, yoff, imxoff, imyoff, imh) { return [(xy[0] - xoff) / xsc + imxoff, imh - ((xy[1] - yoff) / ysc + imyoff)]; }

// calculate the viewport extent in map units
//const vpext_mu = function(l, t, ext, image_wh, vpsz_px) {
//    // l is the pixels from the left of the canvas to the left of the viewport
//    // t is the pixels from the top of the canvas to the top of the viewport
//    var extw = ext[1] - ext[0];
//    var exth = ext[3] - ext[2];
//    var vpext_l = ext[0] + l / image_wh * extw;
//    var vpext_t = ext[3] - t / image_wh * exth;
//    return [vpext_l, vpext_l + vpsz_px[0] / image_wh * extw,
//            vpext_t - vpsz_px[1] / image_wh * exth, vpext_t];
//}

// calculate the viewport extent in map units
const vpext_mu = function(ctr, vpsz_px, xsc, ysc, zoom_level) {
    var w = vpsz_px[0] * xsc / zoom_level; // width in map units
    var h = vpsz_px[1] * ysc / zoom_level;
    return [ctr[0] - w / 2, ctr[0] + w / 2, ctr[1] - h / 2, ctr[1] + h / 2];
}

const redraw_zoomed = function(cm, ext0, zoom_in) {
    // apply the css scale and recentre the viewport, before the server-side redraw (at wider extent for zoom out, or higher res for zoom in)
    // calculate the css offsets that we'll need to apply
    var fx = (cm.viewport_ctr[0] - cm.ext[0]) / (cm.ext[1] - cm.ext[0]); // fraction of x-extent
    var cssx = cm.image_wh * fx - $("#" + cm.id).innerWidth() / 2 + parseInt($("#" + cm.id + "-pannable").css("left"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
    var fy = (cm.ext[3] - cm.viewport_ctr[1]) / (cm.ext[3] - cm.ext[2]); // fraction of y-extent (downwards from top)
    var cssy = cm.image_wh * fy - $("#" + cm.id).innerHeight() / 2 + parseInt($("#" + cm.id + "-pannable").css("top"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
    //console.log("zoom centre css is: " + [cssx, cssy]);
    var ctxlist = window[cm.id + "_ctxlist"];
    var ocv = new OffscreenCanvas(cm.image_wh, cm.image_wh);
    var octx = ocv.getContext('2d');
    // rectangles in source canvas and destination canvas
    if (zoom_in) {
        var srcx = (cm.ext[0] - ext0[0]) / (ext0[1] - ext0[0]) * cm.image_wh; // top-left x coord of source
        var srcy = (ext0[3] - cm.ext[3]) / (ext0[3] - ext0[2]) * cm.image_wh;
        var srcw = cm.image_wh / 2; // or (this.ext[1] - this.ext[0]) / (ext0[1] - ext0[0]) * cm.image_wh;
        var srch = cm.image_wh / 2;
        var destx = 0;
        var desty = 0;
        var destw = cm.image_wh;
        var desth = cm.image_wh;
    } else {
        var destx = (ext0[0] - cm.ext[0]) / (cm.ext[1] - cm.ext[0]) * cm.image_wh; // top-left x coord in destination canvas
        var desty = (cm.ext[3] - ext0[3]) / (cm.ext[3] - cm.ext[2]) * cm.image_wh;
        var destw = cm.image_wh / 2;
        var desth = cm.image_wh / 2;
        //console.log("dest x,y,w,h: " + destx + ", " + desty + ", " + destw + ", " + desth);
        var srcx = 0;
        var srcy = 0;
        var srcw = cm.image_wh;
        var srch = cm.image_wh;
    }
    for (const idx of cm.active_layers) {
        var this_ctx = ctxlist[idx];
        if (!is_canvas_blank(this_ctx.canvas)) {
            // draw to offscreen canvas
            octx.clearRect(0, 0, cm.image_wh, cm.image_wh);
            octx.drawImage(this_ctx.canvas, srcx, srcy, srcw, srch, destx, desty, destw, desth);
            this_ctx.clearRect(0, 0, cm.image_wh, cm.image_wh); // clear the on-screen one
            //console.log(" setting css: " + [-cssx, -cssy]);
            $("#" + cm.id + "-plot" + idx).css({ "left": -cssx, "top": -cssy }); // set the new on-screen css offsets
            this_ctx.drawImage(ocv, 0, 0, cm.image_wh, cm.image_wh); // copy the offscreen one into on-screen
        }
    }
}
