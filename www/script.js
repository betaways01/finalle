window.addEventListener("load", function() {
    const iframes = document.querySelectorAll(".responsive-iframe");
    iframes.forEach(iframe => {
        iframe.style.height = iframe.contentWindow.document.body.scrollHeight + 'px';
    });
});

$(document).on('click', '.clickable', function() {
  var target = $(this).data('target');
  $(target).collapse('toggle');
});