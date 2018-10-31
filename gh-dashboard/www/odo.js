
// separateur millier avec espace//
window.odometerOptions = {
  format: '( ddd).dd'
};

Shiny.addCustomMessageHandler('odo', function(data) {
  var o1 = document.getElementById(data.id);
  o1.innerHTML = data.val;
});
