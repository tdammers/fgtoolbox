var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
components = {
        base: require('./components/base'),
        vornav: require('./components/vornav'),
        wind: require('./components/wind')
    }

$(document).ready(function() {
    var vornav = components.vornav.create()
    var wind = components.wind.create()
    $('body')
        .append($(vornav.elem))
        .append($(wind.elem))
})
