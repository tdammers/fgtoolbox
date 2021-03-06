var R = require('ramda')
var h = require('hyperscript')
var common = require('./common')
var f = require('./form-utils')

module.exports = {
    form: function(data) {
        return h('form.vornav-form',
            h('h3', 'VOR-to-VOR'),
            h('div.column-container',
                h('div',
                    f.section(
                        f.inputRows(
                            {
                                name: 'from',
                                title: 'departure',
                                hint: 'ICAO airport code or NAV ID'
                            },
                            {
                                name: 'to',
                                title: 'destination',
                                hint: 'ICAO airport code or NAV ID'
                            })),
                    f.section(
                        f.buttons(['Route']))),
                h('div.result.form-section')))
    },
    result: function(data) {
        return h('div',
              h('h3',
                'Departure: ',
                data.from.id,
                h('small', ' (', data.from.name, ')')),
              h('h3',
                'Destination: ',
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
