var R = require('ramda')
var h = require('hyperscript')
var common = require('./common')

module.exports = {
    form: function(data) {
        return h('form.wind-form',
            h('h3', 'Wind'),
            h('div.form-section',
                h('h4', 'Aircraft'),
                h('div.form-row',
                    h('label', {'for': 'course'}, 'target course'),
                    h('input', {'type': 'text', 'name': 'course'})),
                h('div.form-row',
                    h('label', {'for': 'airspeed'}, 'airspeed'),
                    h('input', {'type': 'text', 'name': 'airspeed'}))),
            h('div.form-section',
                h('h4', 'Wind'),
                h('div.form-row',
                    h('label', {'for': 'windDir'}, 'direction'),
                    h('input', {'type': 'text', 'name': 'windDir'})),
                h('div.form-row',
                    h('label', {'for': 'windSpeed'}, 'speed'),
                    h('input', {'type': 'text', 'name': 'windSpeed'}))),
            h('div.form-buttons',
                h('button', {'type': 'submit'}, 'Calculate')),
            h('div.result'))
    },
    result: function(data) {
        return h('div',
          h('h3',
            'Required heading: ' + Math.round(data.heading)))
    }
}
