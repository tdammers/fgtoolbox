var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')
var templates = require('./templates')

var prepareVornavForm = function() {
    var spinnerOverlay = $(templates.spinner.spinner())
    $('body').append(spinnerOverlay.hide())

    var vornavForm = $(templates.vornav.form({}))
    $('body').append(vornavForm)

    vornavForm.on('submit', function(e) {
        var form = $(this)
        var from = $('[name=from]', this).val()
        var to = $('[name=to]', this).val()
        var resultbox = $('.result', form)
        spinnerOverlay.fadeIn()
        resultbox.fadeOut()

        var success = function(data) {
            resultbox.empty()
            resultbox.append($(templates.vornav.result(data)).children())
            resultbox.fadeIn()
        }

        var failure = function(data) {
            resultbox.empty()
            resultbox.text(data.responseText)
            resultbox.fadeIn()
        }

        $.get(
            '/api/vornav/' +
            encodeURIComponent(from) +
            '/' +
            encodeURIComponent(to))
            .done(success)
            .fail(failure)
            .always(function(){ spinnerOverlay.fadeOut() })
        e.preventDefault()
        return false
    })
}

$(document).ready(prepareVornavForm)
