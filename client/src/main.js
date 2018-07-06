var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
var components = require('./components')

$(document).ready(function() {
    var vornav = components.vornav.create()
    var wind = components.wind.create()
    var tabber = components.tabber.create([
            {
                "title": "VOR-to-VOR",
                "id": "vornav",
                "component": vornav
            },
            {
                "title": "Wind",
                "id": "wind",
                "component": wind
            }])

    $('body')
        .append($(tabber.elem))
})
