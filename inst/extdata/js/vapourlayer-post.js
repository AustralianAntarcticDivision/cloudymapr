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
            var selmode = window['cm_' + id + '.select_mode'];
            isPan = true;
            didPan = false;
            if (selmode === 'select') {
                var vprect = elViewport.getBoundingClientRect();
                ctx.canvas.height = vprect.height;
                ctx.canvas.width = vprect.width;
                //if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
                // click inside the viewport bounds
                console.log(vprect);
                console.log([ev.clientX, ev.clientY]);
                console.log([ev.pageX, ev.pageY]);
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
                //PP var mvx = ev.movementX; var mvy = ev.movementY; // if panning the viewport. positive X means moving leftwards
                var mvx = ev.movementX; var mvy = ev.movementY; // if panning the layers
                // console.log('pan mX: ' + mvx + ', mY:' + mvy);
                var vpext_mu = cm.vpext_mu();
                // console.log("pan: start vpext_mu is " + vpext_mu);
                vpext_mu[0] = vpext_mu[0] - mvx * cm.xsc;
                vpext_mu[1] = vpext_mu[1] - mvx * cm.xsc;
                vpext_mu[2] = vpext_mu[2] + mvy * cm.ysc;
                vpext_mu[3] = vpext_mu[3] + mvy * cm.ysc;
                // console.log("pan: with move vpext_mu is " + vpext_mu);

                var extend = false; // need to extend the data?
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
                            extend = true;
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
                            extend = true;
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
                            extend = true;
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
                            extend = true;
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
                    if (extend) {
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
                        if (panx) {
                            srcw = cm.image_wh / 2;
                            destw = cm.image_wh / 2;
                            if (mvx > 0) {
                                // leftwards
                                destx = cm.image_wh / 2;
                            } else {
                                srcx = cm.image_wh / 2;
                            }
                        }
                        if (pany) {
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
                        var ctxlist = window[cm.id + "_ctxlist"];
                        var ocv = new OffscreenCanvas(cm.image_wh, cm.image_wh);
                        var octx = ocv.getContext('2d');
                        for (const idx of cm.active_layers) {
                            var this_ctx = ctxlist[idx];
                            if (!is_canvas_blank(this_ctx.canvas)) {
                                // draw to offscreen canvas
                                octx.clearRect(0, 0, cm.image_wh, cm.image_wh);
                                octx.drawImage(this_ctx.canvas, srcx, srcy, srcw, srch, destx, desty, destw, desth);
                                this_ctx.clearRect(0, 0, cm.image_wh, cm.image_wh); // clear the on-screen one
                                console.log(" setting css: " + [-cssx, -cssy]);
                                $("#" + cm.id + "-plot" + idx).css({ "left": -cssx, "top": -cssy }); // set the new on-screen css offsets
                                this_ctx.drawImage(ocv, 0, 0, cm.image_wh, cm.image_wh); // copy the offscreen one into on-screen
                            }
                        }
                        Shiny.setInputValue(id + '-pan_extend', newext);
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
                // clear rectangle from canvas and send selection to server
                var selrect = dragrect;
                var vprect = elViewport.getBoundingClientRect();
                ctx.clearRect(0, 0, vprect.width, vprect.height);
                selrect.startY = vprect.height - selrect.startY; // zero at bottom, not top // TODO fix?
                selrect.endX = selrect.startX + selrect.w;
                selrect.endY = selrect.startY - selrect.h;
                Shiny.setInputValue(id + '-dragselect', [Math.min(selrect.startX, selrect.endX), Math.max(selrect.startX, selrect.endX), Math.min(selrect.startY, selrect.endY), Math.max(selrect.startY, selrect.endY)]); // rectangle in pixels relative to viewport
                dragrect = {};
            }
        }
    };

    elViewport.addEventListener('pointerdown', panStart);
    addEventListener('pointermove', panMove);
    addEventListener('pointerup', panEnd);
};
