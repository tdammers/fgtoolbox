var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
components = {
        base: require('./components/base'),
        vornav: require('./components/vornav')
    }

$(document).ready(function() {
    var vornav = components.vornav.create()
    $('body').append($(vornav.elem))
})
