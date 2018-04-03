var R = require('ramda')
var h = require('hyperscript')
var common = require('./common')

module.exports = {
    form: function(data) {
        return h('form.vornav-form',
            h('h3', 'VOR-to-VOR'),
            h('div.form-row',
                h('label', {'for': 'from'}, 'from'),
                h('input', {'type': 'text', 'name': 'from'})),
            h('div.form-row',
                h('label', {'for': 'to'}, 'to'),
                h('input', {'type': 'text', 'name': 'to'})),
            h('div.form-buttons',
                h('button', {'type': 'submit'}, 'Route')),
            h('div.result'))
    },
    result: function(data) {
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
                            [ common.formatFreq(waypoint.freq)
                            ])
                return h('div',
                    h('strong', waypoint.id),
                    ' (', waypoint.name, ') ',
                    waypointDetails)
                    
              }, data.waypoints))
    }
}
