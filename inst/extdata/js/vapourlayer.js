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
            var selmode = window[id + '_select_mode']; // yuck
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
        var selmode = window[id + '_select_mode']; // yuck
        if (Math.abs(ev.movementX) > 0 || Math.abs(ev.movementY) > 0) { didPan = true; }
//TR        const elImg = elViewport.children[1];
	if (selmode === 'pan') {
            var px2m = window[id + '_px2m']; // yuck
	    for (var ch of elViewport.getElementsByClassName('viewport-pannable')) {
		var new_t = ch.offsetTop + ev.movementY;
		var new_l = ch.offsetLeft + ev.movementX;
	    //TRNS var trns = window.getComputedStyle(ch).translate.replace(/[^0-9\-., ]/g, '').split(' ');
	    //TRNS console.log('trns: ' + trns);
	    //TRNS var new_l = Number(trns[0]) + ev.movementX;
	    //TRNS var new_t = Number(trns[1]) + ev.movementY;
//		console.log('pan: ' + new_l + ', ' + new_t);
                ch.style.top = new_t + 'px'; ch.style.left = new_l + 'px';
//TR                ch.style.transform = 'translate(' + new_l + 'px,' + new_t + 'px)';
            }
            Shiny.setInputValue(id + '-pan_mxy', px2m([ev.movementX, ev.movementY]), { priority: 'event' });
	} else {
            // TODO if the drag goes beyond the viewport bounds, pan it
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
        if (!didPan) {
            var rect = elViewport.getBoundingClientRect();
            if (ev.clientX >= rect.left && ev.clientX <= rect.right && ev.clientY >= rect.top && ev.clientY <= rect.bottom) {
                // click inside the viewport bounds
                var x = ev.clientX - rect.left;
                var y = rect.height - (ev.clientY - rect.top);
                console.log(id + '-mapclick = ', x + ', ' + y + ', ' + event.button);
                Shiny.setInputValue(id + '-mapclick', [x, y, event.button], { priority: 'event' }); // coords of click, in pixels relative to viewport
            }
        } else {
            var selmode = window[id + '_select_mode']; // yuck
	    if (selmode === 'select') {
		// clear rectangle from canvas and send selection to server
		var selrect = dragrect;
		var vprect = elViewport.getBoundingClientRect();
		ctx.clearRect(0, 0, vprect.width, vprect.height);
		selrect.startY = vprect.height - selrect.startY; // zero at bottom, not top
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
