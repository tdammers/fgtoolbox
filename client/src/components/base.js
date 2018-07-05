var R = require('ramda')

var Component = function () {
    this.handlers = {}
    this.children = []
}

Component.prototype.on = function(eventName, handler) {
    this.handlers =
        R.over
            (R.lensProp(eventName))
            (R.append(handler))
            (this.handlers)
}

Component.prototype.raise = function(eventName, data) {
    R.forEach(
        function(f) {
            f(data)
        },
        this.handlers[eventName] || [])
}

Component.prototype.render = function() {
    return ''
}

Component.prototype.addChild = function(child) {
    this.children = R.append(child, this.children)
}

Component.prototype.send = R.curry(function(eventName, data, child) {
    var response = child.on(eventName, data)
    if (R.not(R.isNil(response))) {
        this.raise('childEvent', response)
    }
})

Component.prototype.broadcast = function(eventName, data) {
    R.forEach(this.send.bind(this), this.children)
}

module.exports = Component
