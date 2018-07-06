var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
var components = require('./components')

$(document).ready(function() {
    var vornav = components.vornav.create()
    var wind = components.wind.create()
    $('body')
        .append($(wind.elem))
        .append($(vornav.elem))
})
