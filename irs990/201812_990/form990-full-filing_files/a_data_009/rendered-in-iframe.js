true && function() {
  'use strict';
  if (window.self === window.top) return; // not in iframe

  document.documentElement.style.transformOrigin = '0 0';
  document.documentElement.style.overflowX = 'hidden';
  document.documentElement.style.overflowY = 'hidden';
  document.body.style.marginLeft = '0';

  function getDocHeight(doc) {
    // stackoverflow.com/questions/1145850/
    var body = doc.body, html = doc.documentElement;
    var height = Math.max( body.scrollHeight, body.offsetHeight,
      html.clientHeight, html.scrollHeight, html.offsetHeight );
    return height;
  }

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
      var rect = document.body.getBoundingClientRect();
      var aspect = [
        document.documentElement.scrollWidth,
        document.documentElement.scrollHeight
      ]
      window.parent.postMessage(aspect, '*');
    };
    if (windowLoaded) sendDims();
    else window.onload = sendDims;
  };

  window.onmessage = function receiveStartOrRatio(ev) {
    if (ev.source !== window.parent) return;
    if (ev.data === null) {
      return start();
    }
    // iframes built using srcdoc are only found in a full filing
    var isFull = window.self.location.href.match(/.*srcdoc$/);

    // are we inside a schedule iframe, not child full filing iframe?
    // if so we just scale like normal. this preserves the old
    // functionality which works for single schedules.
    if (!isFull && ev.data && ev.data.type == "ratio") {
      var ratio = ev.data.parentWidth / ev.data.originalWidth;
      document.documentElement.style.transform = 'scale(' + ratio + ')';
      // we need to give the parent the scaled height, now
      var docHeight = getDocHeight(document);
      var message = {
        type: "resized-height",
        height: docHeight,
      };
      window.parent.postMessage(message, '*');
    }
    // backwards compatable for old version of parent full_text.js
    // this code shouldn't be used now that we've deployed the
    // controllers, but is here for posterity
    else if (!isFull && ev.data) {
      document.documentElement.style.transform = 'scale(' + ratio + ')';
    }
    else {
      var originalWidth = document.body.scrollWidth;
      var parentWidth = ev.data.parentWidth - 25;
      var ratio = parentWidth / originalWidth;
      document.documentElement.style.transform = 'scale(' + ratio + ')';
      var message = {
        type: "resized-height",
      };
      window.parent.postMessage(message, '*');
    }
  };
}();
