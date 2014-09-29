var api = window.API_1484_11 = window.API = {};

api.Initialize = function() {
    return true;
};

api.Terminate = function() {
    return true;
};

api.GetValue = function(k) {
    return values[k] || '';
};

var values = {};

var randomUserId = (new Date()).toString() + ' @' + Math.ceil(1000 * Math.random());
values['cmi.learner_id']      = 'Random-Learner-Id: ' + randomUserId;
values['cmi.core.student_id'] = 'Random-Learner-Id: ' + randomUserId;

api.SetValue = function(k, v) {
    values[k] = v;
    return true;
};

api.Commit = function() {
    return true;
};

api.GetLastError = function() {
    return 0;
};

api.GetErrorString = function(e) {
    return 'GetErrorString: ' + e;
};

api.GetDiagnostic = function() {
    return 'GetDiagnostic: ...';
};
