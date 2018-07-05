var R = require('ramda')
var templates = require('../templates')
var $ = require('jquery')

var create = function() {
    var container = $('<div>')
    var cancelHandler = null
    var spinnerOverlay =
            $(templates.spinner.spinner({
                    onCancel: function() {
                        console.log('cancelHandler:', cancelHandler)
                        if (!R.isNil(cancelHandler)) {
                            cancelHandler()
                        }
                    }
                }))
    $(container).append(spinnerOverlay.hide())

    var windForm = $(templates.wind.form({}))
    $(container).append(windForm)

    windForm.on('submit', function(e) {
        var form = $(this)
        var course = $('[name=course]', this).val()
        var windDir = $('[name=windDir]', this).val()
        var windSpeed = $('[name=windSpeed]', this).val()
        var airspeed = $('[name=airspeed]', this).val()
        var resultbox = $('.result', form)
        spinnerOverlay.fadeIn()
        resultbox.fadeOut()

        var success = function(data) {
            if (data.error) {
                return failure(data)
            }
            try {
                resultbox.empty()
                resultbox.append($(templates.wind.result(data)).children())
                resultbox.fadeIn()
            }
            catch (e) {
                return failure(data)
            }
        }

        var failure = function(data) {
            console.log(data)
            resultbox.empty()
            resultbox.text(
                data.message ||
                data.responseText ||
                data.statusText ||
                "Something went wrong")
            resultbox.fadeIn()
        }

        var rq = $.get(
            '/api/wind?' +
            'course=' + encodeURIComponent(course) +
            '&airspeed=' + encodeURIComponent(airspeed) +
            '&wind-dir=' + encodeURIComponent(windDir) +
            '&wind-speed=' + encodeURIComponent(windSpeed))
            .done(success)
            .fail(failure)
            .always(function(){ spinnerOverlay.fadeOut() })

        cancelHandler = function(){
            rq.abort()
            cancelHandler = null
        }
        e.preventDefault()
        return false
    })

    return {
        elem: container
    }
}

module.exports =
    { create: create
    }

