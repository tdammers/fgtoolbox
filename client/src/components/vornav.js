var R = require('ramda')
var templates = require('../templates')
var $ = require('jquery')
var Ajax = require('../ajax-form')

var create = function() {
    var container = $('<div class="container">')
    var vornavForm = $(templates.vornav.form({}))
    $(container).append(vornavForm)

    Ajax.attachHandler(
            vornavForm,
            ['to', 'from'],
            function (ctx) {
                return '/api/vornav/' +
                       encodeURIComponent(ctx.from) +
                       '/' +
                       encodeURIComponent(ctx.to)
            },
            templates.vornav.result)

    return { elem: container }
}

module.exports = {
        create: create
    }
