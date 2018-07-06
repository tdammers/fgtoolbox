var R = require('ramda')

var formatFreq = function (freq) {
    if (R.isNil(freq)) return null;
    if (freq >= 1000) {
        return (freq / 100).toFixed(2) + 'MHz'
    }
    else {
        return freq.toFixed(1) + 'kHz'
    }
}

var formatHeading = function (degrees, precision, unit) {
    if (R.isNil(degrees)) return null;
    var str, len
    if (precision > 0) {
        len = 3
        str = degrees.toFixed(0)
    }
    else {
        len = precision + 4
        str = degrees.toFixed(precision)
    }
    while (str.length < len) str = '0' + str
    str += (R.isNil(unit) ? "°" : unit)
    return str
}

module.exports = {
    formatFreq: formatFreq,
    formatHeading: formatHeading
}
