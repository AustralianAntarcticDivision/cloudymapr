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
        const srcx = (cm.ext[0] - ext0[0]) / (ext0[1] - ext0[0]) * this_ctx.canvas.width; // top-left x coord of source
        const srcy = (ext0[3] - cm.ext[3]) / (ext0[3] - ext0[2]) * this_ctx.canvas.height;
        const srcw = this_ctx.canvas.width / 2; // or (this.ext[1] - this.ext[0]) / (ext0[1] - ext0[0]) * this_ctx.canvas.width;
        const srch = this_ctx.canvas.height / 2;
        const destx = 0;
        const desty = 0;
        const destw = cm.image_wh;
        const desth = cm.image_wh;
    } else {
        const destx = (ext0[0] - cm.ext[0]) / (cm.ext[1] - cm.ext[0]) * this_ctx.canvas.width; // top-left x coord in destination canvas
        const desty = (cm.ext[3] - ext0[3]) / (cm.ext[3] - cm.ext[2]) * this_ctx.canvas.height;
        const destw = this_ctx.canvas.width / 2;
        const desth = this_ctx.canvas.height / 2;
        //console.log("dest x,y,w,h: " + destx + ", " + desty + ", " + destw + ", " + desth);
        const srcx = 0;
        const srcy = 0;
        const srcw = cm.image_wh;
        const srch = cm.image_wh;
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


var cm_$ID$={
    id: "$ID$",
    // functions for transformation from canvas pixels to map units and vice-versa
    xsc: 1, xoff: 0, ysc: 1, yoff: 0, imxoff: 0, imyoff: 0, //xoff, yoff ignored temporarily TODO
    active_layers: [],
//    px2m(xy) { return px2m(xy, this.xsc / this.zoom_level, this.xoff, this.ysc / this.zoom_level, this.yoff, this.imxoff, this.imyoff, this.image_wh); },
//    m2px(xy) { return m2px(xy, this.xsc / this.zoom_level, this.xoff, this.ysc / this.zoom_level, this.yoff, this.imxoff, this.imyoff, this.image_wh); },
    px2m(xy) { return px2m(xy, this.xsc / this.zoom_level, this.ext[1] - this.ext[0], this.ysc / this.zoom_level, this.ext[3] - this.ext[2], this.imxoff, this.imyoff, this.image_wh); }, // probably wrong, fix or remove TODO
    /*m2px(xy, ignore_zoom) {
        if (typeof(ignore_zoom) == "undefined") { ignore_zoom = false; }
        var xsc = this.xsc;
        var ysc = this.ysc;
        var xoff = (this.ext0[1] + this.ext0[0]) / 2;
        var yoff = (this.ext0[3] + this.ext0[2]) / 2;
        if (!ignore_zoom) {
            xsc = xsc / this.zoom_level;
            ysc = ysc / this.zoom_level;
            xoff = (this.ext[1] + this.ext[0]) / 2;
            yoff = (this.ext[3] + this.ext[2]) / 2;
        }
        return m2px(xy, xsc, xoff, ysc, yoff, this.imxoff, this.imyoff, this.image_wh);
    },*/
    m2px(xy) {
        return m2px(xy, this.xsc / this.zoom_level, this.ext[0], this.ysc / this.zoom_level, this.ext[2], 0, 0, this.image_wh);
    },
    vpsz() { return [$('#$ID$').innerWidth() / window.innerWidth, $('#$ID$').innerHeight() / window.innerHeight] }, // viewport size [w,h] as fraction of window size
    vpsz_px() { return [$('#$ID$').innerWidth(), $('#$ID$').innerHeight()] }, // viewport size in px
//     vpext_mu(l, t) {
// //PP         if (typeof(l) == "undefined") { l = parseInt($('#$ID$-pannable').css("left"), 10); } // positive left means the (left side of the) viewport is panned leftwards outside the full map extent
// //PP         if (typeof(t) == "undefined") { t = parseInt($('#$ID$-pannable').css("top"), 10); } // positive top means the (top of the) viewport is panned upwards above top of the full map extent
// 
//         //(-parseInt($("#$ID$-pannable").css("left"), 10) - parseInt($('#$ID$-pannable canvas').first().css("left"), 10)) / cm_$ID$.image_wh // left size of vp wrt to ext, as fraction
//         if (typeof(l) == "undefined") l = -(-parseInt($("#$ID$-pannable").css("left"), 10) - parseInt($('#$ID$-pannable canvas').first().css("left"), 10));
//         if (typeof(t) == "undefined") t = -(-parseInt($("#$ID$-pannable").css("top"), 10) - parseInt($('#$ID$-pannable canvas').first().css("top"), 10));
// /*
//         var extw = this.ext[1] - this.ext[0];
//         var exth = this.ext[3] - this.ext[2];
//         var vpext_l = this.ext[0] - l / this.image_wh * extw;
//         var vpext_t = t / this.image_wh * exth + this.ext[3];
//         return [vpext_l, vpext_l + $('#$ID$').innerWidth() / this.image_wh * extw,
//                 vpext_t - $('#$ID$').innerHeight() / this.image_wh * exth, vpext_t]
// */
//         return vpext_mu(-l, -t, this.ext, this.image_wh, [$('#$ID$').innerWidth(), $('#$ID$').innerHeight()])
//     }, // calculate viewport extent in map units
    vpext_mu() {
        return vpext_mu(this.viewport_ctr, [$('#$ID$').innerWidth(), $('#$ID$').innerHeight()], this.xsc, this.ysc, this.zoom_level);
    },
    select_mode: 'pan',
    w_rsztmr: null,
    w_doneResizing() {
        Shiny.setInputValue('$ID$-window_height', window.innerHeight);
        Shiny.setInputValue('$ID$-window_width', window.innerWidth);
        Shiny.setInputValue('$ID$-view_wh', [$('#$ID$').innerWidth() / window.innerWidth, $('#$ID$').innerHeight() / window.innerHeight]);
    },
    image_wh: 0,
    ext0: [0, 0, 0, 0], // the initial/max extent, we can't pan beyond this
    ext: [0, 0, 0, 0], // current "extended viewport" (i.e. data) extent. If the viewport bounds go outside of this, we need to request data
    res: 0, // spatial grid cell size in map units
    viewport_ctr: [0, 0], // centre of viewport in map units
    update_ext(newext) {
        /* temporarily, just change the ext but don't change the css - TODO this is wrong now with the scaling
           -- will scaled raster with non-scaled vector layers even work when this happens?
        // update the ext value, but also pan the viewport by nudging the css
        var ext0 = this.ext; //.map((x) => x); // current ext
        var xd = (newext[0] - ext0[0]) / this.xsc; // change in x extent in pixels, TODO this is only panning, not zooming
        var yd = (newext[2] - ext0[2]) / this.ysc; // change in y extent in pixels, TODO this is only panning, not zooming
        // if the new extent is left of the current ext, xd will be negative. We want to decrease the css-left attribute value
        var l = parseInt($('#$ID$-pannable').css("left"), 10) + xd; // current left pan adjusted for xd
        console.log("current left-pan: " + parseInt($('#$ID$-pannable').css("left"), 10))
        console.log("new left-pan: " + l);
        $('#$ID$-pannable').css({"left": l + "px"});

        // if the new extent is above the curent ext, yd will be positive
        var t = parseInt($('#$ID$-pannable').css("top"), 10) - yd; // current top pan adjusted for yd
        console.log("current top-pan: " + parseInt($('#$ID$-pannable').css("top"), 10))
        console.log("new top-pan: " + l);
        $('#$ID$-pannable').css({"top": t + "px"});
        */
        this.ext = newext;
        this.viewport_ctr = [(newext[0] + newext[1]) / 2, (newext[2] + newext[3]) / 2];
    },
    zoom_level: 1,
    zoom_in() {
        this.zoom_level = this.zoom_level * 2;
        // adjust ext (data extent)
        var w = (this.ext[1] - this.ext[0]) / 4; var h = (this.ext[3] - this.ext[2]) / 4; // new half-width and half-height
        var ctrx = Math.max(Math.round((this.viewport_ctr[0] - this.ext0[0]) / w), 1) * w + this.ext0[0];
        ctrx = Math.min(ctrx, this.ext0[1] - w);
        var ctry = Math.max(Math.round((this.viewport_ctr[1] - this.ext0[2]) / h), 1) * h + this.ext0[2];
        ctry = Math.min(ctry, this.ext0[3] - h);
        // ctrx and ctry give the centre for the zoom, which won't be the centre of the existing data extent if we have zoomed somewhere not near the original centre
        console.log("data centre for zoom: " + [ctrx, ctry] + ", half-width: " + w + ", viewport centre: ", this.viewport_ctr);
        var ext0 = this.ext.map((x) => x); // copy of unzoomed ext
        this.ext = [ctrx - w, ctrx + w, ctry - h, ctry + h];
        this.res = this.res / 2;
        // change the xsc, ysc etc? TODO (if yes, don't modify those things by zoom level in other funcs)
        // send to server
        Shiny.setInputValue("$ID$-do_zoom", this.ext.concat([this.res, this.zoom_level]).concat(this.viewport_ctr));
        redraw_zoomed(this, ext0, true);
    },
    zoom_out() {
        if (this.zoom_level > 1) {
            this.zoom_level = this.zoom_level / 2;
            var w = (this.ext[1] - this.ext[0]); var h = (this.ext[3] - this.ext[2]); // new half-width and half-height
            var ctrx = Math.max(Math.round((this.viewport_ctr[0] - this.ext0[0]) / w), 1) * w + this.ext0[0];
            ctrx = Math.min(ctrx, this.ext0[1] - w);
            var ctry = Math.max(Math.round((this.viewport_ctr[1] - this.ext0[2]) / h), 1) * h + this.ext0[2];
            ctry = Math.min(ctry, this.ext0[3] - h);
            //console.log("data centre for zoom out: " + [ctrx, ctry] + ", width: " + w + ", viewport centre: ", this.viewport_ctr);
            var ext0 = this.ext.map((x) => x); // copy of unzoomed ext
            this.ext = [ctrx - w, ctrx + w, ctry - h, ctry + h];
            //console.log("zoomed ext: " + this.ext + ", original ext: " + ext0);
            this.res = this.res * 2;
            // send to server
            Shiny.setInputValue("$ID$-do_zoom", this.ext.concat([this.res, this.zoom_level]).concat(this.viewport_ctr));
            redraw_zoomed(this, ext0, false);
        }
    },

    // centre the viewport on xy (specified in map units) and pan the child canvas elements appropriately
    /* this version works with a delta method, so if we get out of whack we never recover
      set_vpctr_mu(xy) {
        // get current vp ctr
        var ch = $("#$ID$-pannable canvas"); // all child canvas elements
        //var l = (-parseInt($("#$ID$-pannable").css("left"), 10) - parseInt(ch.first().css("left"), 10));
        //var t = (-parseInt($("#$ID$-pannable").css("top"), 10) - parseInt(ch.first().css("top"), 10));
        var delta = [xy[0] - this.viewport_ctr[0], xy[1] - this.viewport_ctr[1]]; // offset in map units to apply
        console.log("vpctr map units is: " + this.viewport_ctr + ", delta is: ", delta);
        delta = [delta[0] / (this.xsc / this.zoom_level), delta[1] / (this.ysc / this.zoom_level)]; // in pixels
        console.log("delta pixels is: ", delta);
        ch.each( function(idx, el) {
            $(this).css({ "left": parseInt($(this).css("left"), 10) - delta[0], "top": parseInt($(this).css("top"), 10) + delta[1] });
        });
        this.viewport_ctr = xy;
    },*/

    // set the viewport centre to `xy` (specified in map units) and pan the image canvases appropriately
    set_vpctr_mu(xy) {
        var fx = (xy[0] - this.ext[0]) / (this.ext[1] - this.ext[0]); // fraction of x-extent
        var cssx = this.image_wh * fx - $('#$ID$').innerWidth() / 2 + parseInt($("#$ID$-pannable").css("left"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
        var fy = (this.ext[3] - xy[1]) / (this.ext[3] - this.ext[2]); // fraction of y-extent (downwards from top)
        var cssy = this.image_wh * fy - $('#$ID$').innerHeight() / 2 + parseInt($("#$ID$-pannable").css("top"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
        console.log("centre css is: " + [cssx, cssy]);
        var ch = $("#$ID$-pannable canvas"); // all child canvas elements
        ch.each( function(idx, el) {
            $(this).css({ "left": -cssx, "top": -cssy });
        });
        this.viewport_ctr = xy;
    },
}

Shiny.initializedPromise.then(function() {
    Pannable(document.querySelector('#$ID$'));
    Shiny.setInputValue('$ID$-window_height', window.innerHeight); Shiny.setInputValue('$ID$-window_width', window.innerWidth);
    /* on startup, send the viewport size info */
    Shiny.setInputValue('$ID$-view_wh', cm_$ID$.vpsz());
    $('#$ID$-zoom_in').on('pointerdown', function(ev) { ev.preventDefault(); cm_$ID$.zoom_in(); });
    $('#$ID$-zoom_out').on('pointerdown', function(ev) { ev.preventDefault(); cm_$ID$.zoom_out(); });
    /* when the window resizes, re-send the viewport size information */
    $(window).resize(function() {
        if (cm_$ID$.w_rsztmr) clearTimeout(cm_$ID$.w_rsztmr);
        cm_$ID$.w_rsztmr = setTimeout(cm_$ID$.w_doneResizing, 200);
    });
    Shiny.setInputValue('$ID$-request_init', 1);
});
