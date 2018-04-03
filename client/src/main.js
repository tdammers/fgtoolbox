var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
var components = {
        vornav: require('./components/vornav')
    }

$(document).ready(function() {
    var vornav = components.vornav.create()
    $('body').append($(vornav.elem))
})
