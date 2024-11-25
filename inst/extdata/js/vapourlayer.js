var $ID$_w_scaling=0; var $ID$_h_scaling=0; var $ID$_select_mode='pan';
function $ID$_m2px(mxy) { return [Math.round(mxy[0] * $ID$_w_scaling), Math.round(mxy[1] * $ID$_h_scaling)] };
function $ID$_px2m(pxy) { return [pxy[0] / $ID$_w_scaling, pxy[1] / $ID$_h_scaling] };
/* helper function to send the viewport size [w,h] as fraction of window size */
function $ID$_vpsz() { return [$('#$ID$').innerWidth() / window.innerWidth, $('#$ID$').innerHeight() / window.innerHeight] };

$(document).on('shiny:sessioninitialized', function() {
    Pannable(document.querySelector('#$ID$'));
    Shiny.setInputValue('$ID$-window_height', window.innerHeight); Shiny.setInputValue('$ID$-window_width', window.innerWidth);
    /* on startup, send the viewport size info */
    Shiny.setInputValue('$ID$-view_wh', $ID$_vpsz());
    $('#$ID$-zoom_in').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('$ID$-do_zoom', [2, -parseInt($('#$ID$-pannable').css('left')), -parseInt($('#$ID$-pannable').css('top'))], { priority: 'event' }); });
    $('#$ID$-zoom_out').on('pointerdown', function(ev) { ev.preventDefault(); Shiny.setInputValue('$ID$-do_zoom', [0.5, -parseInt($('#$ID$-pannable').css('left')), -parseInt($('#$ID$-pannable').css('top'))], { priority: 'event' }); });
    /* when the window resizes, re-send the viewport size information */
    var $ID$_w_rsztmr;
    $(window).resize(function() {
        clearTimeout($ID$_w_rsztmr);
        $ID$_w_rsztmr = setTimeout($ID$_w_doneResizing, 500);
    });
    function $ID$_w_doneResizing() {
        Shiny.setInputValue('$ID$-window_height', window.innerHeight); Shiny.setInputValue('$ID$-window_width', window.innerWidth); Shiny.setInputValue('$ID$-view_wh', $ID$_vpsz());
    }}); /* do we also need to watch the viewport for resizing? Can it be resized independently of the window? */
