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
                        [ 'course', 'target course' ],
                        [ 'airspeed', 'airspeed' ])),
                f.section('Wind',
                    f.inputRows(
                        [ 'windDir', 'direction' ],
                        [ 'windSpeed', 'speed' ]))
            ),
            f.section(null,
                f.buttons(['Calculate'])),
            h('div.result.form-section'))
    },
    result: function(data) {
        return h('div',
          h('h3',
            'Required heading: ' + Math.round(data.heading)))
    }
}
