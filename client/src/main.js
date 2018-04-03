console.log('hi')
var $ = require('jquery')
var R = require('ramda')
var h = require('hyperscript')

var formatFreq = function(freq) {
    if (R.isNil(freq)) return null;
    if (freq >= 1000) {
        return (freq / 100).toFixed(2) + 'MHz'
    }
    else {
        return freq.toFixed(1) + 'kHz'
    }
}

var tplVornavResult = function(data) {
    return h('div',
          h('h3',
            'From ',
            data.from.id,
            h('small', ' (', data.from.name, ')'),
            ' to ',
            data.to.id,
            h('small', ' (', data.to.name, ')')),
          h('h4', data.dist.toFixed(1), ' nm'),
          h('pre',
            R.intersperse(' ')(R.pluck('id')(data.waypoints))),
          R.map(function(waypoint){
            var waypointDetails =
                    R.reject(R.isNil)(
                        [ formatFreq(waypoint.freq)
                        ])
            return h('div',
                h('strong', waypoint.id),
                ' (', waypoint.name, ') ',
                waypointDetails)
                
          }, data.waypoints))
}

var tplSpinner = function() {
    return h('div',
          h('div.spinnerOverlay',
            h('div.spinnerContainer',
              h('div.spinner',
                h('span', '\u2708')),
              h('div', 'Please wait...'))))
}

var prepareVornavForm = function() {
    var spinnerOverlay = $(tplSpinner()).children()
    $('body').append(spinnerOverlay.hide())

    $('form[data-submit-proc]').on('submit', function(e) {
        var form = $(this)
        var from = $('[name=from]', this).val()
        var to = $('[name=to]', this).val()
        var resultbox = $('.result', form)
        spinnerOverlay.fadeIn()
        resultbox.fadeOut()

        var success = function(data) {
            resultbox.empty()
            resultbox.append($(tplVornavResult(data)).children())
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
