/**
 * Full Filing Script
 *
 * This script gets run from the iframe that contains
 * all of the individual schedules. Because of that,
 * I call this a "mid-level" script in the overall
 * full text rendering scheme.
 *
 * What this does is handle the navigator drop down,
 * hide the iframes until they're loaded and resizes
 * them based on their actual height.
 *
 * This also listens for resizing messages posted
 * from the full_text.js script, intercepting width
 * and height values from the parent page (the main
 * NPE full text page, not this iframe). We pass this
 * information down to each of the children iframes
 * so they have accurate values to resize and scale
 * themselves to.
 *
 * Lastly, this script listens for resize messages
 * coming from the iframe_script (rendered-in-iframe.js)
 * and resizes the height of the iframes according to
 * their new height. Once its done this, it passes
 * the new iframe height to the top-level full_text
 * script so this iframe will be resized to fit the
 * new contents.
 */
function getDocHeight(doc) {
  // stackoverflow.com/questions/1145850/
  var body = doc.body, html = doc.documentElement;
  var height = Math.max( body.scrollHeight, body.offsetHeight,
    html.clientHeight, html.scrollHeight, html.offsetHeight );
  return height;
}

function setIframeHeight(ifrm) {
  var doc = ifrm.contentDocument? ifrm.contentDocument:
    ifrm.contentWindow.document;
  ifrm.style.visibility = 'hidden';
  ifrm.style.height = "10px"; // reset to minimal height ...
  // IE opt. for bing/msn needs a bit added or scrollbar appears
  ifrm.style.height = getDocHeight( doc ) + 4 + "px";
  ifrm.style.width = "100%";
  ifrm.style.visibility = 'visible';
}

/** Scrolls the element into view
 * Manually created since Safari does not support the native one inside an iframe
 * behavior is one of: smooth, instant, auto (default: auto)
 * TODO: Test this across many mobile browsers/devices before employing it.
 */
function scrollElementIntoView(element, behavior) {
  var scrollTop = window.pageYOffset || element.scrollTop;
  // Furthermore, if you have for example a header outside the iframe
  // you need to factor in its dimensions when calculating the position to scroll to
  //var headerOutsideIframe = window.parent.document.getElementsByClassName('myHeader')[0].clientHeight;
  var finalOffset = element.getBoundingClientRect().top + scrollTop; // + headerOutsideIframe;
  window.parent.scrollTo({
    top: finalOffset,
    behavior: behavior || 'auto'
  });
}

function scrollToFiling() {
  event.preventDefault();
  event.stopPropagation();
  var formName = document.querySelector("#IRSfilingSelector").value;
  var formIndex = 0;
  // There are two ways to address a form/index pair:
  // IRS990ScheduleA (0 index implied) or IRS990ScheduleA/2 (second index)
  var withIndex = formName.match("([A-Za-z0-9]+)/([A-Za-z0-9]*)");
  if (withIndex) {
    formName = withIndex[1];
    formIndex = Number.parseInt(withIndex[2], 10);
  }
  document.querySelector(`#${formName}_${formIndex}`).scrollIntoView();
}

function scrollToTop() {
  document.querySelector("#IRSfilingSelector").selectedIndex = 0;
  document.querySelector("#topSelector").scrollIntoView();
}

window.onload = () => {
  document.querySelectorAll("iframe").forEach((ifrm) => {
    setIframeHeight(ifrm);
  });
  document.querySelectorAll(".goto-link").forEach((a) => {
    a.style.visibility = 'visible';
  });
  // Tell the top level script what our new height is.
  // This is for the initial load.
  var ratio = [
    document.documentElement.scrollWidth,
    document.documentElement.scrollHeight
  ];
  window.parent.postMessage(ratio, '*');

  window.addEventListener('message', function receiveDimensions(ev) {
    if (!ev.data || !ev.data.type) return;
    // The top-level parent script has told us how tall and
    // wide the page is. This gets passed to children so they
    // can scale accordingly.
    if (ev.data.type == "ratio") {
      if (ev.data.parentWidth < 800) {
        document.querySelector("#topSelector").style.visibility = 'hidden';
        document.querySelector("#topSelector").style.height = '0px';
      }
      document.querySelectorAll("iframe").forEach(function relayMsg(iframe) {
        iframe.contentWindow.postMessage(ev.data, '*');
      });
    }
    // A child has rescaled itself and is telling us this
    // has happened. Here' we make the iframes big enough
    // to fit all content. Then we send a message upward
    // telling the top level parent script how tall this
    // full filing iframe content is.
    else if (ev.data.type == "resized-height") {
      document.querySelectorAll("iframe").forEach((iframe) => {
        var ifrmBody = iframe.contentDocument.body;
        var rect = ifrmBody.getBoundingClientRect();
        //iframe.contentDocument.body.style.height =  rect.height + "px";
        var correctHeight = rect.height + 25 + "px";
        iframe.style.height = correctHeight;
      });
      var message = {
        type: "resized-height",
        height: document.body.scrollHeight,
      };
      window.parent.postMessage(message, '*');
    }
  });
};

window.scrollToTop = scrollToTop;
window.scrollToFiling = scrollToFiling;
