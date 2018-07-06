var R = require('ramda')
var templates = require('../templates')
var $ = require('jquery')
var Ajax = require('../ajax-form')

var create = function(tabInfos) {
    var container = $('<div class="tabber">')
    var tabStrip = $('<div class="tabber-strip">').appendTo(container)
    var tabBody = $('<div class="tabber-body">').appendTo(container)

    var activateTab = function (id) {
        return function () {
            $('.tabber-tab', container).removeClass("active")
            $('.tabber-tab[data-tab-id="' + id + '"]', container).addClass("active")
            $('.tabber-content', container).hide()
            $('.tabber-content[data-tab-id="' + id + '"]', container).show()
        }
    }

    R.map(function (tabInfo) {
            var tab = $('<div class="tabber-tab">')
                            .text(tabInfo.title)
                            .on('click', activateTab(tabInfo.id))
                            .attr('data-tab-id', tabInfo.id)
                            .appendTo(tabStrip)
            var content = $('<div class="tabber-content">')
                            .append(tabInfo.component.elem)
                            .attr('data-tab-id', tabInfo.id)
                            .appendTo(tabBody)
                            .hide()
        }, tabInfos)

    return { elem: container }
}

module.exports = {
    create: create
}
