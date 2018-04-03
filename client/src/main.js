console.log('hi')
var $ = require('jquery')
var R = require('ramda')

var expandTemplate = function(template, context) {
    var target = $(template).clone(true, true)
    $('[data-tpl-value]', target).each(function() {
        var key = $(this).attr('data-tpl-value')
        var val = context[key] || ''
        $(this).text(val)
    })
    return target.children()
}

$(document).ready(function(){
    $('form[data-submit-proc]').on('submit', function(e) {
        var form = $(this)
        var from = $('[name=from]', this).val()
        var to = $('[name=to]', this).val()
        var spinnerOverlay = expandTemplate('#tplSpinner')
        form.append(spinnerOverlay)
        var resultbox = $('.result', form)

        var success = function(data) {
            resultbox.empty()
            resultbox
                .append(
                    expandTemplate('#tplVornavResult',
                        {
                            'from-id': data.from.id,
                            'from-name': data.from.name,
                            'to-id': data.to.id,
                            'to-name': data.to.name
                        }))
            resultbox
                .append(
                    $('<h4>')
                        .text(
                            data.dist.toFixed(1) +
                            'nm'))
            R.forEach(
                function(waypoint){
                    resultbox
                        .append(
                            $('<div>')
                                .append(
                                    $('<strong>')
                                        .text(waypoint.id)))
                },
                data.waypoints)
            console.log(data)
        }

        var failure = function(data) {
            resultbox.empty()
            resultbox.text(data.responseText)
        }

        $.get(
            '/api/vornav/' +
            encodeURIComponent(from) +
            '/' +
            encodeURIComponent(to))
            .done(success)
            .fail(failure)
            .always(function(){ spinnerOverlay.remove() })
        e.preventDefault()
        return false
    })
})
