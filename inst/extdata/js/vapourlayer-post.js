async function pan_canvases(cm, srcx, srcy, srcw, srch, destx, desty, destw, desth, cssx, cssy) {
    var ctxlist = window[cm.id + "_ctxlist"];
    var ocv = new OffscreenCanvas(cm.image_wh, cm.image_wh);
    var octx = ocv.getContext('2d');
    for (const idx of cm.active_layers) {
        var this_ctx = ctxlist[idx];
        if (!is_canvas_blank(this_ctx.canvas, this_ctx)) {
            // draw to offscreen canvas
            octx.clearRect(0, 0, cm.image_wh, cm.image_wh);
            octx.drawImage(this_ctx.canvas, srcx, srcy, srcw, srch, destx, desty, destw, desth);
            this_ctx.clearRect(0, 0, cm.image_wh, cm.image_wh); // clear the on-screen one
            console.log(" setting css of layer " + idx + ": " + [-cssx, -cssy]);
            $("#" + cm.id + "-plot" + idx).css({ "left": -cssx, "top": -cssy }); // set the new on-screen css offsets
            this_ctx.drawImage(ocv, 0, 0, cm.image_wh, cm.image_wh); // copy the offscreen one into on-screen
        }
    }
    return true;
}

const Pannable = (elViewport) => {
    // modified from https://stackoverflow.com/questions/68280184/panning-image-when-overflow-scroll
    elViewport.addEventListener('contextmenu', (ev) => { ev.preventDefault(); }); // enable right-click on the viewport
    let isPan = false;
    let didPan = false;
    let dragrect = {};
    const canvas = elViewport.getElementsByClassName("viewport-canvas")[0]; // or use id-canvas or even elViewport.firstElementChild, but that would be fragile to html changes
    const ctx = canvas.getContext('2d');

    const panStart = (ev) => {
        ev.preventDefault();
        var rect = elViewport.getBoundingClientRect();
        if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
            // click was inside viewport bounds
            var id = elViewport.getAttribute('id');
            var cm = window['cm_' + id];
            var selmode = cm.select_mode;
            isPan = true;
            didPan = false;
            if (selmode === 'select') {
                var vprect = elViewport.getBoundingClientRect();
                ctx.canvas.height = vprect.height;
                ctx.canvas.width = vprect.width;
                //if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
                // click inside the viewport bounds
                var x = ev.clientX - vprect.left;
                var y = ev.clientY - vprect.top;
                dragrect.startX = x;
                dragrect.startY = y;
            }
        }
    };

    const panMove = (ev) => {
        if (!isPan) return;
        var id = elViewport.getAttribute('id');
        var cm = window['cm_' + id];
        if (Math.abs(ev.movementX) > 0 || Math.abs(ev.movementY) > 0) { didPan = true; }
        if (cm.select_mode === 'pan') {
            var ch = $("#" + id + "-pannable canvas"); // all child canvas elements
//NOPV            var ch = $("#" + id + " .viewport-image"); // all child canvas elements
            if (ch.length > 0) {
                var mvx = ev.movementX; var mvy = ev.movementY;
                // console.log('pan mX: ' + mvx + ', mY:' + mvy);
                var vpext_mu = cm.vpext_mu();
                // console.log("pan: start vpext_mu is " + vpext_mu);
                vpext_mu[0] = vpext_mu[0] - mvx * cm.xsc;
                vpext_mu[1] = vpext_mu[1] - mvx * cm.xsc;
                vpext_mu[2] = vpext_mu[2] + mvy * cm.ysc;
                vpext_mu[3] = vpext_mu[3] + mvy * cm.ysc;
                // console.log("pan: with move vpext_mu is " + vpext_mu);

                var extendx = false; var extendy = false; // need to extend the data?
                var panx = false; var pany = false; // allow the pan? (not beyond allowable extent)
                var dx = (cm.ext[1] - cm.ext[0]) / 2;
                var dy = (cm.ext[3] - cm.ext[2]) / 2;
                var newext = cm.ext.map((x) => x); // clone the ext
                if (mvx > 0) {
                    // console.log("pan left");
                    // leftwards movement
                    if (vpext_mu[0] >= cm.ext0[0]) {
                        // not beyond allowable extent
                        panx = true;
                        if (vpext_mu[0] < cm.ext[0]) {
                            // console.log("extend");
                            // beyond current data extent, extend
                            newext[0] = newext[0] - dx;
                            newext[1] = newext[1] - dx;
                            extendx = true;
                        }
                    }
                } else if (mvx < 0) {
                    if (vpext_mu[1] <= cm.ext0[1]) {
                        // not beyond allowable extent
                        panx = true;
                        if (vpext_mu[1] > cm.ext[1]) {
                            // beyond current data extent, extend
                            newext[0] = newext[0] + dx;
                            newext[1] = newext[1] + dx;
                            extendx = true;
                        }
                    }
                }
                if (mvy < 0) {
                    // downwards movement
                    // console.log("pan down");
                    if (vpext_mu[2] >= cm.ext0[2]) {
                        pany = true;
                        if (vpext_mu[2] < cm.ext[2]) {
                            // console.log("extend");
                            newext[2] = newext[2] - dy;
                            newext[3] = newext[3] - dy;
                            extendy = true;
                        }
                    }
                } else if (mvy > 0) {
                    // console.log("pan up");
                    if (vpext_mu[3] <= cm.ext0[3]) {
                        pany = true;
                        if (vpext_mu[3] > cm.ext[3]) {
                            // console.log("extend");
                            newext[2] = newext[2] + dy;
                            newext[3] = newext[3] + dy;
                            extendy = true;
                        }
                    }
                }
                if (panx || pany) {
                    if (panx) { cm.viewport_ctr[0] = cm.viewport_ctr[0] - mvx * cm.xsc / cm.zoom_level; }
                    if (pany) { cm.viewport_ctr[1] = cm.viewport_ctr[1] + mvy * cm.ysc / cm.zoom_level; }
                    ch.each( function(idx, el) {
                        var l = parseInt($(this).css("left"), 10);
                        var t = parseInt($(this).css("top"), 10);
                        if (panx && pany) {
                            $(this).css({ "left": l + mvx, "top": t + mvy });
                        } else if (panx) {
                            $(this).css("left", l + mvx);
                        } else {
                            $(this).css("top", t + mvy);
                        }
                    });
                    if (extendx || extendy) {
                        console.log("pan extend from: " + cm.ext + " to: " + newext);
                        // replot the canvases and set the panned extent now
                        cm.ext = newext;
                        var srcx = 0; var srcy = 0; var destx = 0; var desty = 0;
                        var srcw = cm.image_wh; var destw = cm.image_wh;
                        var srch = cm.image_wh; var desth = cm.image_wh;
                        var fx = (cm.viewport_ctr[0] - newext[0]) / (newext[1] - newext[0]); // fraction of x-extent
                        var cssx = cm.image_wh * fx - $("#" + cm.id).innerWidth() / 2 + parseInt($("#" + cm.id + "-pannable").css("left"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
                        var fy = (newext[3] - cm.viewport_ctr[1]) / (newext[3] - newext[2]); // fraction of y-extent (downwards from top)
                        var cssy = cm.image_wh * fy - $("#" + cm.id).innerHeight() / 2 + parseInt($("#" + cm.id + "-pannable").css("top"), 10); // take off half the viewport width to get the left side, and adjust for the left-offset of the parent
                        console.log("pan extend css is: " + [cssx, cssy]);
                        if (extendx) {
                            srcw = cm.image_wh / 2;
                            destw = cm.image_wh / 2;
                            if (mvx > 0) {
                                // leftwards
                                destx = cm.image_wh / 2;
                            } else {
                                srcx = cm.image_wh / 2;
                            }
                        }
                        if (extendy) {
                            srch = cm.image_wh / 2;
                            desth = cm.image_wh / 2;
                            if (mvy > 0) {
                                // upwards
                                desty = cm.image_wh / 2;
                            } else {
                                srcy = cm.image_wh / 2;
                            }
                        }
                        console.log("pan copy from: " + [srcx, srcy, srcw, srch] + " to " + [destx, desty, destw, desth]);
                        Shiny.setInputValue(id + '-pan_extend', newext);
                        pan_canvases(cm, srcx, srcy, srcw, srch, destx, desty, destw, desth, cssx, cssy); // don't wait for it, but note that it's still running in the same (main) thread
                    }
                }
            }
        } else {
            // TODO if the drag goes beyond the viewport bounds, pan it, but not beyond the max allowable extent
            var vprect = elViewport.getBoundingClientRect();
            ctx.clearRect(0, 0, vprect.width, vprect.height);
            ctx.fillStyle = '#00008080';
            dragrect.w = ev.clientX - vprect.left - dragrect.startX;
            dragrect.h = ev.clientY - vprect.top - dragrect.startY;
            ctx.setLineDash([5]);
            ctx.lineWidth = 1;
            ctx.strokeStyle = '#000080';
            ctx.fillRect(dragrect.startX, dragrect.startY, dragrect.w, dragrect.h);
            ctx.strokeRect(dragrect.startX, dragrect.startY, dragrect.w, dragrect.h);
            //console.log([dragrect.startX, dragrect.startY, dragrect.w, dragrect.h]);
        }
    };

    const panEnd = (ev) => {
        isPan = false;
        var id = elViewport.getAttribute('id');
        var cm = window['cm_' + id];
        if (!didPan) {
            var rect = elViewport.getBoundingClientRect();
            if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
                // click inside the viewport bounds
                var xy = [ev.clientX - rect.left, ev.clientY - rect.top]; // coords of click, in pixels relative to left-top of viewport
                // console.log("mapclick (px rel to vp): " + xy);
                xy = [xy[0] - parseInt($("#" + cm.id + "-plot1").css("left"), 10) - parseInt($("#" + cm.id + "-pannable").css("left"), 10),
                      xy[1] - parseInt($("#" + cm.id + "-plot1").css("top"), 10) - parseInt($("#" + cm.id + "-pannable").css("top"), 10)];
                // console.log("mapclick (px rel to canvas): " + xy);
                var xy_mu = [cm.ext[0] + xy[0] / cm.image_wh * (cm.ext[1] - cm.ext[0]), cm.ext[3] - xy[1] / cm.image_wh * (cm.ext[3] - cm.ext[2])];
                // console.log("mapclick (mu): " + xy_mu);
                Shiny.setInputValue(id + '-mapclick', xy_mu.concat(xy, event.button), { priority: 'event' }); // coords of click in map units, then in pixels relative to canvas TL plus button number
            }
        } else {
            if (cm.select_mode === 'select') {
                // clear rectangle from canvas
                var selrect = dragrect;
                var vprect = elViewport.getBoundingClientRect();
                ctx.clearRect(0, 0, vprect.width, vprect.height);
                // don't zoom if the dragrect was too small
                // TODO a better way of aborting the drag-selection
                if (Math.abs(selrect.w) > 1 && Math.abs(selrect.h) > 1) {
                    selrect.startY = vprect.height - selrect.startY; // zero at bottom, not top // TODO fix?
                    selrect.endX = selrect.startX + selrect.w;
                    selrect.endY = selrect.startY - selrect.h;
                    // that's the selected region in pixels relative to viewport, noting that the y-values are zero at the bottom, not top
                    var selrect2 = [Math.min(selrect.startX, selrect.endX), Math.max(selrect.startX, selrect.endX), Math.min(selrect.startY, selrect.endY), Math.max(selrect.startY, selrect.endY)];
                    // and convert y-values to positive downwards from top, not positive upwards from bottom
                    selrect2[2] = vprect.height - selrect2[2];
                    selrect2[3] = vprect.height - selrect2[3];
                    // console.log("selrect2: " + selrect2.map(Math.round));
                    const selrect2_px = [
                        selrect2[0] - parseInt($("#" + cm.id + "-plot1").css("left"), 10) - parseInt($("#" + cm.id + "-pannable").css("left"), 10),
                        selrect2[1] - parseInt($("#" + cm.id + "-plot1").css("left"), 10) - parseInt($("#" + cm.id + "-pannable").css("left"), 10),
                        selrect2[2] - parseInt($("#" + cm.id + "-plot1").css("top"), 10) - parseInt($("#" + cm.id + "-pannable").css("top"), 10),
                        selrect2[3] - parseInt($("#" + cm.id + "-plot1").css("top"), 10) - parseInt($("#" + cm.id + "-pannable").css("top"), 10)
                    ]; // in pixels relative to canvas, noting that y-pixels here are measured down from the top of the canvas
                    // console.log("selrect2_px: " + selrect2_px.map(Math.round));
                    const tl_mu = cm.px2m([selrect2_px[0], selrect2_px[2]]);
                    const br_mu = cm.px2m([selrect2_px[1], selrect2_px[3]]);
                    const selrect2_mu = [tl_mu[0], br_mu[0], br_mu[1], tl_mu[1]]; // xmin xmax ymin ymax
                    // console.log("selrect2_mu: " + selrect2_mu.map(Math.round));
                    const sel_cxy = [(selrect2_mu[0] + selrect2_mu[1]) / 2, (selrect2_mu[2] + selrect2_mu[3]) / 2]; // zoom centre in mu
                    // console.log("zoom ctr: " + sel_cxy.map(Math.round));
                    // now figure out what zoom we want. We want to zoom to the lowest power of 2 that encompasses the range of this mu rectangle, with the appropriate alignment to tile edges
                    // NOTING that we want the new viewport to show the zoomed extent as best as possible, meaning that the zoom level should not come from cm.ext but instead cm.vpext_mu()
                    const vpext_mu = cm.vpext_mu();
                    const zx = Math.pow(2, Math.floor(Math.log2(Math.abs(vpext_mu[1] - vpext_mu[0]) / Math.abs(selrect2_mu[1] - selrect2_mu[0]))));
                    const zy = Math.pow(2, Math.floor(Math.log2(Math.abs(vpext_mu[3] - vpext_mu[2]) / Math.abs(selrect2_mu[3] - selrect2_mu[2]))));
                    // console.log("zoom: " + zx + " or " + zy);
                    // choose the lowest, unless one is 1 and the other is larger
                    var zz;
                    if (zx < 2 && zy < 2) {
                        zz = 1;
                    } else if (zx < 2) {
                        zz = zy;
                    } else if (zy < 2) {
                        zz = zx;
                    } else {
                        zz = Math.min(zx, zy);
                    }
                    cm.zoom_in(zz, sel_cxy);
                }
                dragrect = {};
            }
        }
    };

    elViewport.addEventListener('pointerdown', panStart);
    addEventListener('pointermove', panMove); // NB not attached to elViewport, so that movement beyond the viewport is captured TODO check
    elViewport.addEventListener('pointerup', panEnd);
};
