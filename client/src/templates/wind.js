var R = require('ramda')
var h = require('hyperscript')
var common = require('./common')
var f = require('./form-utils')

module.exports = {
    form: function(data) {
        return h('form.wind-form',
            h('h3', 'Wind'),
            h('div.column-container',
                f.section('Aircraft',
                    f.inputRows(
                        [ 'course', 'target course', 'text', 'bearing', 'degrees or N/S/W/E/...' ],
                        [ 'airspeed', 'airspeed', 'text', 'speed', 'KTAS' ])),
                f.section('Wind',
                    f.inputRows(
                        [ 'windDir', 'direction', 'text', 'bearing', 'degrees or N/S/W/E/...' ],
                        [ 'windSpeed', 'speed', 'text', 'speed', 'KTAS' ])),
                h('div.result.form-section')
            ),
            f.section(null,
                f.buttons(['Calculate'])))
    },
    result: function(data) {
        return h('div',
          h('h3', 'Heading: ', common.formatHeading(data.heading, 0)),
          h('div',
                'To track course ',
                h('strong',
                    common.formatHeading(data.request.course)),
                ' with wind ',
                h('strong',
                    common.formatHeading(data.request.windDir, 0, '') + "@" + data.request.windSpeed.toFixed(0)),
                ', you need a heading of ',
                h('strong',
                    common.formatHeading(data.heading, 2))))
    }
}
