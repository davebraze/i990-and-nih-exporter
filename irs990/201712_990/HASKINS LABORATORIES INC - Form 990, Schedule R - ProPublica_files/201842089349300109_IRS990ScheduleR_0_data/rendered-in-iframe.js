true && function() {
  'use strict';
  if (window.self === window.top) return; // not in iframe

  document.documentElement.style.transformOrigin = '0 0';
  document.documentElement.style.overflowX = 'hidden';
  document.body.style.marginLeft = '0';

  var windowLoaded = false; // if parsing this, definitionally not loaded yet
  window.onload = function noteLoad() {
    windowLoaded = true;
  };

  var started = false;
  var start = function startFunc() {
    if (started) return;
    started = true;

    var sendDims = function sendDimsFunc() {
      // These measurements are only good if everything has loaded
      // (And the iframe must be very small in the parent!)
      window.parent.postMessage([
        document.documentElement.scrollWidth,
        document.documentElement.scrollHeight
      ], '*');
    };
    if (windowLoaded) sendDims();
    else window.onload = sendDims;
  };

  window.onmessage = function receiveStartOrRatio(ev) {
    if (ev.source !== window.parent) return;

    if (ev.data === null) start();
    else document.documentElement.style.transform = 'scale(' + ev.data + ')';
  };
}();
