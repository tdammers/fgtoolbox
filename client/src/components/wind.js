var R = require('ramda')
var templates = require('../templates')
var $ = require('jquery')
var Ajax = require('../ajax-form')

var create = function() {
    var container = $('<div>')
    var windForm = $(templates.wind.form({}))
    $(container).append(windForm)

    Ajax.attachHandler(
        windForm,
        ['course', 'windDir', 'windSpeed', 'airspeed'],
        function (ctx) {
            return '/api/wind?' +
                   'course=' + encodeURIComponent(ctx.course) +
                   '&airspeed=' + encodeURIComponent(ctx.airspeed) +
                   '&wind-dir=' + encodeURIComponent(ctx.windDir) +
                   '&wind-speed=' + encodeURIComponent(ctx.windSpeed)
        },
        templates.wind.result)

    return { elem: container }
}

module.exports =
    {
        create: create
    }

