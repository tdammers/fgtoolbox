var R = require('ramda')
var h = require('hyperscript')

var inputRow = function (options) {
    var name, label, type, dataType, hint
    if (typeof(options) === 'string') {
        name = options
        label = arguments[1] || name
        type = arguments[2] || 'text'
        dataType = arguments[3] || 'text'
        hint = arguments[4] || ''
    }
    else if (Array.isArray(options)) {
        name = options[0] || ''
        label = options[1] || name
        type = options[2] || 'text'
        dataType = options[3] || 'text'
        hint = options[4] || ''
    }
    else {
        name = options['name'] || ''
        label = options['label'] || name
        type = options['type'] || 'text'
        dataType = options['dataType'] || 'text'
        hint = options['hint'] || ''
    }

    return h('div.form-row',
        h('label', {'for': name}, label),
        h('input', {'type': type, 'name': name, 'data-type': dataType, 'placeholder': hint}))
}

var inputRows = function () {
    var defs = arguments
    return R.map(inputRow, defs)
}

var button = function (def) {
    var text, options

    if (typeof(def) === 'string') {
        text = def
        options = {'type': 'submit'}
    }
    else {
        text = def['text']
        options = def
    }
    
    return h('button', options, text)
}


var buttons = function () {
    var defs = arguments
    return h('div.form-buttons', R.map(button, defs))
}

var section = function (/* title, content ... */) {
    var title = R.head(arguments)
    var content = R.tail(arguments)
    return h('div.form-section',
        title === null ? null : h('h3', title),
        content)
}

module.exports.button = button
module.exports.buttons = buttons
module.exports.inputRow = inputRow
module.exports.inputRows = inputRows
module.exports.section = section
