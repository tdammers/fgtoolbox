var R = require('ramda')
var h = require('hyperscript')
var common = require('./common')

module.exports = {
    spinner: function() {
      return h('div.spinnerOverlay',
        h('div.spinnerContainer',
          h('div.spinner',
            h('span', '\u2708')),
          h('div', 'Please wait...')))
    }
}
