var R = require('ramda')
var $ = require('jquery')
var templates = require('./templates')

var attachHandler = function (form, names, makeRequestURL, template) {
    var container = $(form).parent('.container')
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

    var submitHandler = function (e) {
        var form = $(this)
        var getFormVal = function (name) {
                    var elem = $('[name=' + name + ']', form)
                    var value = elem.val()
                    switch (elem.attr('data-type')) {
                        case 'bearing':
                            switch (value) {
                                case 'N': return 0
                                case 'NE': return 45
                                case 'E': return 90
                                case 'SE': return 135
                                case 'S': return 180
                                case 'SW': return 225
                                case 'W': return 270
                                case 'NW': return 315
                                default: return Number(value)
                            }
                        case 'speed':
                        case 'number':
                            return Number(value)
                        default:
                            return value
                    }
                }
        var rqData = R.zipObj(names, R.map(getFormVal, names))
        var resultbox = $('.result', form)
        spinnerOverlay.fadeIn()

        var success = function(data) {
            if (data.error) {
                return failure(data)
            }
            try {
                resultbox.empty()
                resultbox.append($(template(R.assoc("request", rqData, data))).children())
                resultbox.fadeIn()
            }
            catch (e) {
                return failure(data)
            }
        }

        var failure = function(data) {
            resultbox.empty()
            resultbox.text(
                data.message ||
                data.responseText ||
                data.statusText ||
                "Something went wrong")
            resultbox.fadeIn()
        }

        var rq = $.get(makeRequestURL(rqData))
            .done(success)
            .fail(failure)
            .always(function(){ spinnerOverlay.fadeOut() })

        cancelHandler = function(){
            rq.abort()
            cancelHandler = null
        }
        e.preventDefault()
        return false
    }

    $(form).on('submit', submitHandler)
}

module.exports.attachHandler = attachHandler
