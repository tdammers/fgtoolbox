var R = require('ramda')

var formatFreq = function(freq) {
    if (R.isNil(freq)) return null;
    if (freq >= 1000) {
        return (freq / 100).toFixed(2) + 'MHz'
    }
    else {
        return freq.toFixed(1) + 'kHz'
    }
}

module.exports = {
    formatFreq: formatFreq
}
