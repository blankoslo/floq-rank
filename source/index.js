const Elm = require('./Main');

require('elm-ui/stylesheets/main.scss');
require('./stylesheets/main.scss');

var app = Elm.Main.embed(document.getElementById('app'), {
  apiToken: window.apiToken,
  apiHost: window.config && window.config.apiUri
});
