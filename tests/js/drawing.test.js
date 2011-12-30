load('../../js/raphael.js');
load('../../js/drawing.js');

var contextTestSuite = function() {
    function test_can_project_vertical_position() {
	ctx = new diego.drawing.Context({}, 400, 500, new Date(2011, 00, 01), 'http://localhost:500&id=');
	var when = new Date(2011, 00, 01, 12, 00, 00);
	var pos = ctx.getRelativePosition(when);
	jsUnity.assertions.assertEqual(250 + ctx.topOffset, pos);
    }

    function test_can_parse_emacs_date() {
	ctx = new diego.drawing.Context({}, 400, 500, new Date(2011, 00, 01), 'http://localhost:500&id=');
	var dt = ctx.parseDate('2010-11-21 Tue 12:35');
	jsUnity.assertions.assertEqual(new Date(2030, 10, 21, 12, 35), dt);
    }

    function test_can_parse_normal_date() {
	ctx = new diego.drawing.Context({}, 400, 500, new Date(2011, 00, 01), 'http://localhost:500&id=');
	var dt = ctx.parseDate('2011-03-06 09:03');
	jsUnity.assertions.assertEqual(new Date(2011, 02, 06, 09, 03), dt);
    }
};

suites.push(contextTestSuite);

var stuffTestSuite = function() {
    function test_do_stuff() {
	jsUnity.assertions.assertEqual(3, 2);
    }
};

suites.push(stuffTestSuite);