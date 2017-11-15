/* eslint no-console:0 */
// This file is automatically compiled by Webpack, along with any other files
// present in this directory. You're encouraged to place your actual application logic in
// a relevant structure within app/javascript and only use these pack files to reference
// that code so it'll be compiled.
//
// To reference this file, add <%= javascript_pack_tag 'application' %> to the appropriate
// layout file, like app/views/layouts/application.html.erb

window.addEventListener("play", function (evt) {
    if (window.$_currentlyPlaying && window.$_currentlyPlaying != evt.target) {
        window.$_currentlyPlaying.pause();
    }
    window.$_currentlyPlaying = evt.target;
}, true);

var elem = document.querySelector('.masonry');
var msnry = new Masonry(elem, {
    // options
    itemSelector: '.masonry-item',
    columnWidth: 200
});
