
// Fade-in animation trigger
Shiny.addCustomMessageHandler('addFadeIn', function(message) {
  var el = document.getElementById(message.id);
  if (!el) return;
  el.classList.remove('fade-in');
  void el.offsetWidth; // reflow
  el.classList.add('fade-in');
});
