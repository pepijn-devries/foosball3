shinyjs.updateSideStyle = function(params) {
  document.documentElement.style.setProperty(
    '--bg-ftable-side' + params.side,
    params.color_bg);
  document.documentElement.style.setProperty(
    '--border-ftable-side' + params.side,
    params.color_border);
};
