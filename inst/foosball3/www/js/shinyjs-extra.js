shinyjs.updateSideStyle = function(params) {
  document.documentElement.style.setProperty(
    '--bg-ftable-side' + params.side,
    params.color_bg);
  document.documentElement.style.setProperty(
    '--border-ftable-side' + params.side,
    params.color_border);
};

shinyjs.speech_supported = function(params) {
  var result = ('speechSynthesis' in window);
  Shiny.setInputValue(params.id, result);
};

shinyjs.announce = function(params) {
  if ('speechSynthesis' in window) {
    var announce = new SpeechSynthesisUtterance(params.speech);
    if (announce) {
      announce.lang = params.lang; // 'nl-NL';
      announce.pitch = params.pitch; // 0.9;
      announce.rate = params.rate; // 0.8;
      window.speechSynthesis.speak(announce);
    }
  }
};
